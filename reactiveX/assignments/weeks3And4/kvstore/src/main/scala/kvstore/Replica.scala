package kvstore

import akka.actor.{Actor, ActorRef, OneForOneStrategy, PoisonPill, Props, SupervisorStrategy, Terminated}
import akka.actor.Timers
import kvstore.Arbiter._
// import akka.pattern.{ask, pipe}
import scala.concurrent.duration._
// import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class UpdateTimedOut(id: Long, requester: ActorRef)
  case object RetryOutstandingPersistence

  case class UpdateOperationStatus(
    client: ActorRef,
    key: String,
    valueOption: Option[String],
    persisted: Boolean,
    awaitingReplicationFrom: Set[ActorRef])

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor with Timers {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  val persister = context.actorOf(persistenceProps)
  context.watch(this.persister)
  timers.startTimerAtFixedRate("Retry Persistence", RetryOutstandingPersistence, 100.milliseconds)

  // I added this
  override def preStart(): Unit = {
    arbiter ! Join
  }

  override val supervisorStrategy = OneForOneStrategy() {
    // Restart Persistence Actor if it throws an exception
    case _: PersistenceException => SupervisorStrategy.Restart
  }

  def receive = {
    case JoinedPrimary   => context become leader
    case JoinedSecondary => context become replica(0L)
  }

  var updateOperations = Map.empty[Long, UpdateOperationStatus]

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) =>
      timers.startSingleTimer(id, UpdateTimedOut(id, sender), 1.second)
      this.updateOperations += (id -> UpdateOperationStatus(sender, key, Some(value), false, this.replicators))
      this.kv += (key -> value)
      this.persister ! Persist(key, Some(value), id)
      this.replicators foreach { _ ! Replicate(key, Some(value), id) }

    case Remove(key, id) =>
      timers.startSingleTimer(id, UpdateTimedOut(id, sender), 1.second)
      this.updateOperations += (id -> UpdateOperationStatus(sender, key, None, false, this.replicators))
      this.kv -= key
      this.persister ! Persist(key, None, id)
      this.replicators foreach { _ ! Replicate(key, None, id) }

    case Get(key, id) =>
      sender ! GetResult(key, this.kv.get(key), id)

    case Replicas(replicas) =>
      val replicatorsToStop = for {
        (replica, replicator) <- this.secondaries
        if !replicas.contains(replica)
      } yield replicator
      this.updateOperations = this.updateOperations map {
        case (id, UpdateOperationStatus(client, key, valueOption, persisted, replicators)) =>
          id -> UpdateOperationStatus(client, key, valueOption, persisted, replicators -- replicatorsToStop)
      }
      replicatorsToStop foreach { _ ! PoisonPill }
      this.updateOperations filter {
        case (_, UpdateOperationStatus(_, _, _, _, replicators)) => replicators.isEmpty
      } foreach {
        case (id, UpdateOperationStatus(client, _, _, _, _)) =>
          client ! OperationAck(id)
      }
      this.updateOperations = this.updateOperations filter {
        case (_, UpdateOperationStatus(_, _, _, _, replicators)) => replicators.nonEmpty
      }

      this.secondaries = (for {
        replica <- replicas
        if replica != self
        replicator = this.secondaries get replica match {
          case Some(repl) => repl
          case None =>
            val newReplicator = context.actorOf(Replicator.props(replica))
            this.replicators += newReplicator
            // Does the id in the Replicate message matter? No.
            this.kv foreach { case (k, v) => newReplicator ! Replicate(k, Some(v), 0L) }
            newReplicator
        }
      } yield replica -> replicator).toMap

    case Persisted(_, id) =>
      this.updateOperations get id foreach {
        case UpdateOperationStatus(client, key, valueOption, _, replicators) =>
          if (replicators.isEmpty) {
            this.updateOperations -= id
            timers.cancel(id)
            client ! OperationAck(id)
          } else {
            this.updateOperations += (id -> UpdateOperationStatus(client, key, valueOption, true, replicators))
          }
      }

    case Replicated(_, id) =>
      this.updateOperations get id foreach {
        case UpdateOperationStatus(client, key, valueOption, persisted, replicators) =>
          val remainingReplicators = replicators - sender
          if (remainingReplicators.isEmpty) {
            this.updateOperations -= id
            timers.cancel(id)
            client ! OperationAck(id)
          } else {
            this.updateOperations += (id -> UpdateOperationStatus(client, key, valueOption, persisted, remainingReplicators))
          }
      }

    case RetryOutstandingPersistence =>
      this.updateOperations foreach {
        case (id, UpdateOperationStatus(_, key, valueOption, persisted, _)) =>
          if (!persisted) this.persister ! Persist(key, valueOption, id)
      }

    case Terminated =>
      this.updateOperations foreach {
        case (id, UpdateOperationStatus(_, key, valueOption, _, _)) =>
          this.persister ! Persist(key, valueOption, id)
      }

    case UpdateTimedOut(id, client) =>
      if (this.updateOperations.isDefinedAt(id)) {
        this.updateOperations -= id
        timers.cancel(id)
        client ! OperationFailed(id)
      }
  }

  var snapshotsToReplicator = Map.empty[Long, ActorRef]
  // I changed this to def replica(seq: Long) from val replica
  /* TODO Behavior for the replica role. */
  def replica(expectedSeq: Long): Receive = {
    case Get(key, id) =>
      sender ! GetResult(key, this.kv get key, id)

    case Snapshot(key, valueOption, seq) =>
      if (seq == expectedSeq) {
        valueOption match {
          case Some(value) =>
            this.kv += (key -> value)
          case None =>
            this.kv -= key
        }
        this.persister ! Persist(key, valueOption, seq)
        timers.startTimerWithFixedDelay(seq, Persist(key, valueOption, seq), 100.milliseconds)
        this.snapshotsToReplicator = this.snapshotsToReplicator + (seq -> sender)
        context become replica(expectedSeq + 1)
      } else if (seq < expectedSeq) {
        sender ! SnapshotAck(key, seq)
      }

    case m @ Persist(_, _, _) =>
      this.persister ! m

    case Persisted(key, seq) =>
      this.snapshotsToReplicator get seq foreach { replicator =>
        timers.cancel(seq)
        replicator ! SnapshotAck(key, seq)
        this.snapshotsToReplicator -= seq
      }
  }

}

