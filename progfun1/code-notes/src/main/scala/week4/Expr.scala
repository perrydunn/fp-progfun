package week4

trait Expr {
  def eval: Int =
    this match {
      case Var(_) => throw new Error("Var(String).eval")
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
      case Prod(e1, e2) => e1.eval * e2.eval
    }

  def show: String =
    this match {
      case Var(x) => x
      case Number(n) => n.toString
      case Prod(e1, e2) => (e1, e2) match {
        case (e1: Sum, e2: Sum) =>
          "(" + e1.show + ") * (" + e2.show + ")"
        case (e1: Sum, e2) =>
          "(" + e1.show + ") * " + e2.show
        case (e1, e2: Sum) =>
          e1.show + " * (" + e2.show + ")"
        case (e1, e2) =>
          e1.show + " * " + e2.show
      }
      case Sum(e1, e2) => e1.show + " + " + e2.show
    }
}

case class Var(x: String) extends Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
