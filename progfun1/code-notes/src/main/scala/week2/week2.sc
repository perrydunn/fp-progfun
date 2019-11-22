// Rewrite sum from linear recursion to tail recursion
def sum(f: Int => Int, a: Int, b: Int): Int = {
  def recSum(x: Int, acc: Int): Int =
    if (x > b) acc else recSum(x + 1, acc + f(x))
  recSum(a, 0)
}

sum(x => x, 1, 4)
sum(x => x * x, 1, 3)

// Curried sum
// def sum(f: Int => Int)(a: Int, b: Int): Int =
//   if (a > b) 0 else f(a) + sum(f)(a + 1, b)

// Product version of sum
def product(f: Int => Int)(a: Int)(b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1)(b)

def factorial: Int => Int = product(x => x)(1)
// or def factorial(n: Int): Int = product(x => x)(1, n) if
// used the form of sum above.

factorial(3)
factorial(5)

def series(op: (Int, Int) => Int, init: Int)(f: Int => Int)(a: Int)(b: Int): Int =
  if (a > b) init else op(f(a), product(f)(a + 1)(b))

def isProduct: (Int => Int) => Int => Int => Int = series(_ * _, 1)

def isFact: Int => Int = isProduct(x => x)(1)

isFact(3)
isFact(5)

// Slides version...
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

// Using cheeky _ here instead of (x, y) => x * y
def isSum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, _ + _, 0)(a, b)

isSum(x => x)(1, 3)

// Fixed Point (Newton Round 2)

val tolerance = 0.0001

def abs(x: Double) = if (x < 0) -x else x

def isCloseEnough(x: Double, y: Double): Boolean =
  abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

def sqrt(x: Double): Double = fixedPoint(averageDamp(y => x / y))(1.0)

sqrt(4)

// Data Structures

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

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x - y - z  // -79/42
