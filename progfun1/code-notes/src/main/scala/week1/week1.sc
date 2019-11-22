// Newton's Method Square Root

def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
    (guess + x / guess) / 2

  def sqrtIter(guess: Double): Double = {
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))
  }

  sqrtIter(1.0)
}

sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)

// Tail Recursive Factorial
def factorial(n: Int): Int = {
  def recFact(n: Int, acc: Int): Int =
    if (n == 0) acc else recFact(n - 1, n * acc)
  recFact(n, 1)
}

factorial(10)
