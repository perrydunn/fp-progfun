abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

// Persistent Data Structure: those branches unaffected are reused!
// This allows fun prog to scale up (no unnecessary copies).

class NonEmpty(node: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int) =
    if (x > node) new NonEmpty(node, left, right incl x)
    else if (x < node) new NonEmpty(node, left incl x, right)
    else this

  def contains(x: Int) =
    if (x == node) true
    else if (x > node) right contains x
    else left contains x

  def union(other: IntSet) =
    ((left union right) union other) incl node

  override def toString =
    "[" + left.toString + node + right.toString + "]"
}

object Empty extends IntSet {
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)
  def contains(x: Int) = false
  def union(other: IntSet) = other
  override def toString = "."
}

val eg = new NonEmpty(1, Empty, Empty)
eg contains 1
eg contains 2
val eg2 = eg.incl(2)
val eg3 = eg2.incl(0)
val eg4 = new NonEmpty(4, Empty, Empty)
eg3 union eg4


// Implementation in abstract class
abstract class Base {
  def foo = 1
  def bar: Int
}
class Sub extends Base {
  override def foo = 2
  def bar = 1
}
