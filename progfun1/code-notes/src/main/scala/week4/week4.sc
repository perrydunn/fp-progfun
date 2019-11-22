// Decomposition
trait Expr {
  // Classifiers
  def isNumber: Boolean
  def isSum: Boolean
  // Accessors
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  def isNumber = true
  def isSum = false
  def numValue = n
  def leftOp = throw new Error("Number.leftOp")
  def rightOp = throw new Error("Number.rightOp")
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def isNumber = false
  def isSum = true
  def numValue = throw new Error("Sum.numValue")
  def leftOp = e1
  def rightOp = e2
}

def eval(e: Expr): Int = {
  if (e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression " + e.toString)
}
// This gets tedious!
// => Pattern Matching FTW

import week4.{Var, Number, Sum, Prod}

Sum(Number(2), Number(3)).show
Var("x").show
Prod(Var("x"), Sum(Number(3), Var("y"))).show
Prod(Sum(Number(2), Var("x")), Var("y")).show
Prod(Sum(Number(2), Var("x")), Sum(Number(3), Var("y"))).show
Sum(Prod(Number(2), Var("x")), Prod(Number(3), Var("y"))).show

// List in Scala standard library
// Insertion sort
def isort(xs: scala.List[Int]): scala.List[Int] = {
  def insert(x: Int, xs: scala.List[Int]): scala.List[Int] =
    xs match {
      case scala.List() => scala.List(x)
      case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
    }
  xs match {
    case scala.List() => scala.List()
    case y :: ys => insert(y, isort(ys))
  }
}

isort(scala.List(3, 4, 1, 2, 0))
