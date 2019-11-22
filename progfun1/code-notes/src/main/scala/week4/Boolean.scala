package week4.idealized.scala

abstract class Bool {
  // if (cond) te else ee  becomes
  // cond.ifThenElse(te, ee)
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => Bool): Bool = ifThenElse(x, False)
  def || (x: => Bool): Bool = ifThenElse(True, x)
  def unary_! : Bool

  def == (x: Bool): Bool = ifThenElse(x, x.unary_!)
  def != (x: Bool): Bool = ifThenElse(x.unary_!, x)

  def < (x: Bool): Bool = ifThenElse(False, x)
}

object True extends Bool {
  def ifThenElse[T](t: => T, e: => T) = t
  def unary_! = False
}

object False extends Bool {
  def ifThenElse[T](t: => T, e: => T) = e
  def unary_! = True
}
