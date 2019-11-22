package week6

// So can run in REPL (`sbt console`) since IntelliJ not printing
// newline "\n" on worksheet execution output.
//import week6.Queens.{queens, courseQueens, show}
//show(queens(4))

object Queens {
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

  def courseQueens(n: Int): Set[List[Int]] = {
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
}
