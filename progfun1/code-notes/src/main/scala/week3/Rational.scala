package week3

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be non-zero!")
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  def numer = x
  def denom = y

  def unary_- = new Rational(-numer, denom)

  def <= (that: Rational) =
    that.numer * denom > numer * that.denom

  def max(that: Rational) =
    if (this <= that) that else this

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def - (that: Rational) = this + -that

  override def toString = {
    val g = gcd(numer, denom)
    (numer / g).toString + "/" + (denom / g).toString
  }
}
