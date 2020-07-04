/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional (used to stash incoming operations during garbage collection)
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context become garbageCollecting(newRoot)
    }
    case op: Operation => root ! op
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished => {
      root = newRoot
      pendingQueue foreach (root ! _)
      pendingQueue = Queue.empty[Operation]
      context become normal
    }
    case op: Operation => pendingQueue = pendingQueue enqueue op
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  /**
   * Acknowledges that a copy has been completed. This message should be sent
   * from a node to its parent, when this node and all its children nodes have
   * finished being copied.
   */
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, e) =>
      if (e == this.elem) {
        removed = false
        requester ! OperationFinished(id)
      } else if (e < this.elem) {
        subtrees.get(Left) match {
          case Some(node) => node ! Insert(requester, id, e)
          case None => {
            val node = context.actorOf(BinaryTreeNode.props(e, initiallyRemoved = false))
            subtrees = subtrees + (Left -> node)
            requester ! OperationFinished(id)
          }
        }
      } else {
        subtrees.get(Right) match {
          case Some(node) => node ! Insert(requester, id, e)
          case None => {
            val node = context.actorOf(BinaryTreeNode.props(e, initiallyRemoved = false))
            subtrees = subtrees + (Right -> node)
            requester ! OperationFinished(id)
          }
        }
      }
    case Contains(requester, id, e) =>
      if (e == this.elem) {
        if (!removed) {
          requester ! ContainsResult(id, result = true)
        } else {
          requester ! ContainsResult(id, result = false)
        }
      } else if (e < this.elem) {
        subtrees.get(Left) match {
          case Some(node) => node ! Contains(requester, id, e)
          case None => requester ! ContainsResult(id, result = false)
        }
      } else {
        subtrees.get(Right) match {
          case Some(node) => node ! Contains(requester, id, e)
          case None => requester ! ContainsResult(id, result = false)
        }
      }
    case Remove(requester, id, e) =>
      if (e == this.elem) {
        removed = true
        requester ! OperationFinished(id)
      } else if (e < this.elem) {
        subtrees.get(Left) match {
          case Some(node) => node ! Remove(requester, id, e)
          case None => requester ! OperationFinished(id)
        }
      } else {
        subtrees.get(Right) match {
          case Some(node) => node ! Remove(requester, id, e)
          case None => requester ! OperationFinished(id)
        }
      }
    case CopyTo(treeNode) => {
      if (removed && subtrees.isEmpty) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        subtrees foreachEntry ((_, node) => node ! CopyTo(treeNode))
        if (!removed) {
          treeNode ! Insert(self, elem, elem)
          context become copying(Set.from(subtrees.values), insertConfirmed = false)
        } else {
          context become copying(Set.from(subtrees.values), insertConfirmed = true)
        }
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(_) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        context become copying(expected, insertConfirmed = true)
      }
    case CopyFinished =>
      val nowExpected = expected - sender
      if (nowExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        context become copying(nowExpected, insertConfirmed)
      }
  }

}
