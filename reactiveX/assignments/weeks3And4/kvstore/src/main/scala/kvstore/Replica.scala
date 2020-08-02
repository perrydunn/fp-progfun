package kvstore

import akka.actor.{Actor, ActorRef, OneForOneStrategy, PoisonPill, Props, SupervisorStrategy, Terminated}
import akka.actor.Timers
import kvstore.Arbiter._
import scala.concurrent.duration._

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

  case class UpdateTimedOut(id: Long)
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

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  val persister = context.actorOf(persistenceProps)
  context.watch(this.persister)
  timers.startTimerAtFixedRate("Retry Persistence", RetryOutstandingPersistence, 100.milliseconds)

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

  // Map from ID to its status
  var updateOperations = Map.empty[Long, UpdateOperationStatus]

  def performUpdateOperation(key: String, valueOption: Option[String], id: Long) = {
    timers.startSingleTimer(id, UpdateTimedOut(id), 1.second)
    valueOption match {
      case Some(value) => this.kv += (key -> value)
      case None => this.kv -= key
    }
    this.updateOperations += (id -> UpdateOperationStatus(sender, key, valueOption, false, this.replicators))
    this.persister ! Persist(key, valueOption, id)
    this.replicators foreach { _ ! Replicate(key, valueOption, id) }
  }

  def retryOutstandingPersistence() = this.updateOperations foreach {
    case (id, UpdateOperationStatus(_, key, valueOption, persisted, _)) =>
      if (!persisted) this.persister ! Persist(key, valueOption, id)
  }

  def completePersistence(id: Long, messageToSend: Any) = this.updateOperations get id foreach {
    case UpdateOperationStatus(client, key, valueOption, _, replicators) =>
      if (replicators.isEmpty) {
        this.updateOperations -= id
        timers.cancel(id)
        client ! messageToSend
      } else {
        this.updateOperations += (id -> UpdateOperationStatus(client, key, valueOption, true, replicators))
      }
  }

  val leader: Receive = {
    case Insert(key, value, id) =>
      performUpdateOperation(key, Some(value), id)

    case Remove(key, id) =>
      performUpdateOperation(key, None, id)

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

    case Persisted(_, id) => completePersistence(id, OperationAck(id))

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

    case RetryOutstandingPersistence => retryOutstandingPersistence()

    case Terminated =>
      this.updateOperations foreach {
        case (id, UpdateOperationStatus(_, key, valueOption, _, _)) =>
          this.persister ! Persist(key, valueOption, id)
      }

    case UpdateTimedOut(id) =>
      this.updateOperations get id foreach {
        case UpdateOperationStatus(client, _, _, _, _) => client ! OperationFailed(id)
      }
  }

  def replica(expectedSeq: Long): Receive = {
    case Get(key, id) =>
      sender ! GetResult(key, this.kv get key, id)

    case Snapshot(key, valueOption, seq) =>
      if (seq == expectedSeq) {
        performUpdateOperation(key, valueOption, seq)
        context become replica(expectedSeq + 1)
      } else if (seq < expectedSeq) {
        sender ! SnapshotAck(key, seq)
      }

    case Persisted(key, seq) => completePersistence(seq, SnapshotAck(key, seq))

    case RetryOutstandingPersistence => retryOutstandingPersistence()
  }

}
