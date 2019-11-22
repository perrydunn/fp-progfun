val M = 5
val N = 4
(1 to M) flatMap (x => (1 to N) map (y => (x, y)))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ case (x, y) => x * y }.sum

scalarProduct(Vector(1, 0, 1), Vector(-1, 1, 1))


// Prime Number tester (abstract over efficiency)
def verbosePrime(n: Int): Boolean = n match {
  case x if x < 2 => false
  case x => {
    def existsRemainder(y: Int): Boolean = {
      if (y > n / 2) false
      else if (x % y == 0) true
      else existsRemainder(y + 1)
    }
    if (existsRemainder(2)) false else true
  }
}

verbosePrime(3)
verbosePrime(4)
verbosePrime(5)
verbosePrime(12)
verbosePrime(13)

// Far more concisely, using operations available...
def isPrime(n: Int): Boolean =
  (2 until n) forall (d => n % d != 0)

isPrime(3)
isPrime(4)
isPrime(5)
isPrime(12)
isPrime(13)


// Replacing Loops with Data Structures and For-Expressions (not For-Loops!)
val n = 7
(1 until n) map (i => (1 until i) map (j => (i, j)))
// flatten is like (xss foldRight Seq[Int]())(_ ++ _)
((1 until n) map (i => (1 until i) map (j => (i, j)))).flatten
// or, more succinctly
(1 until n) flatMap (i => (1 until i) map (j => (i, j)))

// So to find pairs where sum is prime...
(1 until n) flatMap (i => (1 until i) map (j => (i, j))) filter{ case (x, y) => isPrime(x + y) }

// For-Expression can make this more readable!
// Unlike For-Loop, no side effects, as produces (yields) new result.
// eg
case class Person(name: String, age: Int)
val persons = List(Person("Adam", 21), Person("Beth", 23), Person("Mani", 32))
for ( p <- persons if p.age < 30 ) yield p.name
// equivalent to
persons filter (p => p.age < 30) map (p => p.name)

// so, back to our prime number example...
for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

// Exercise: scalarProduct using a for-expression
def newScalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum

newScalarProduct(Vector(1, 0, 1), Vector(-1, 1, 1))


// N-Queens (using Sets)
def queens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int, queens: List[Int]): Boolean = queens match {
    case Nil => true
    case q :: qs =>
      if (q >= col - 1 && q <= col + 1) false
      else if (qs contains col) false
      else true
  }
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  placeQueens(n)
}

queens(3)
queens(4)
queens(5)

// Given isSafe
def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens
  queensWithRow forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}

def show(solutions: Set[List[Int]]): String = {
  val displaySolutions = solutions map (
    queens =>
      for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  ) map (solution => solution mkString "\n")
  displaySolutions mkString "\n\n"
}

// IntelliJ not printing new lines!
show(queens(4))


// Maps
class VerbosePolynomial(val terms: Map[Int, Double]) {
  def + (other: VerbosePolynomial) =
    new VerbosePolynomial(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    this.terms get exp match {
      case Some(c) => (exp, c + coeff)
      case None => term
    }
  }

  override def toString = (
    for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff + "x^" + exp
    ) mkString " + "
}

val poly1 = new VerbosePolynomial(Map(1 -> 1, 0 -> 3, 3 -> 2))
val poly2 = new VerbosePolynomial(Map(1 -> 3, 0 -> 1, 2 -> 1))
poly1 + poly2


class Polynomial(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  def + (other: Polynomial) =
    new Polynomial(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString = (
    for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff + "x^" + exp
    ) mkString " + "
}

val p1 = new Polynomial(1 -> 1, 0 -> 3, 3 -> 2)
val p2 = new Polynomial(1 -> 3, 0 -> 1, 2 -> 1)
p1 + p2


class FoldPolynomial(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  def + (other: FoldPolynomial) =
    new FoldPolynomial((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))
  }

  override def toString = (
    for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff + "x^" + exp
    ) mkString " + "
}

val fp1 = new FoldPolynomial(1 -> 1, 0 -> 3, 3 -> 2)
val fp2 = new FoldPolynomial(1 -> 3, 0 -> 1, 2 -> 1)
fp1 + fp2
