package week3

import java.util.NoSuchElementException

// Cons-List
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  def nth(n: Int): T =
    if (n < 0) throw new IndexOutOfBoundsException("Index out of range.")
    else if (n == 0) head
    else tail.nth(n - 1)
  override def toString = {
    def stringify(list: List[T]): String =
      if (list.tail.isEmpty) list.head.toString
      else list.head.toString + ", " + stringify(list.tail)
    "[" + stringify(this) + "]"
  }
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int) = throw new IndexOutOfBoundsException("Index out of range.")
  override def toString = "[]"
}

object List {
  def apply[T]() = new Nil[T]
  def apply[T](elem0: T): List[T] = new Cons[T](elem0, new Nil[T])
  def apply[T](elem0: T, elem1: T): List[T] = new Cons[T](elem0, new Cons[T](elem1, new Nil[T]))
}
