def sqrtStream(x: Double): LazyList[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: LazyList[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(4).take(10).toList

/*
def take(n: Int): Stream[T] = {
  if (isEmpty) this
  else cons(head, tail.take(n - 1))
}
 */

/*
def toList: List[T] = this match {
  case empty => Nil
  case _ => head :: tail.toList
}
 */

// (1 #:: (guesses map improve)).take(10).toList
// Stream.cons(1, guesses map improve).take(10).toList
// Stream.cons(1, guesses map improve).take(10).head :: Stream.cons(1, guesses map improve).take(10).tail.toList
// Stream.cons(1, (guesses map improve).take(9)).head :: Stream.cons(1, guesses map improve).take(10).tail.toList
// 1 :: Stream.cons(1, guesses map improve).take(10).tail.toList
// 1 :: Stream.cons(1, (guesses map improve).take(9)).tail.toList
// 1 :: (guesses map improve).take(9).toList
// 1 :: Stream.cons(1, guesses map improve).map(improve).take(9).toList
// 1 :: Stream.cons(1, guesses map improve).map(improve).take(9).head :: Stream.cons(1, guesses map improve).map(improve).take(9).tail.toList
// 1 :: Stream.cons(Stream.cons(1, guesses map improve).map(improve).head, Stream.cons(1, guesses map improve).map(improve).take(8)).head :: ...
// 1 :: Stream.cons(Stream.cons(improve(1), guesses map improve map improve).head, Stream.cons(1, guesses map improve).map(improve).take(8)).head :: ..
// 1 :: Stream.cons(improve(1), Stream.cons(1, guesses map improve).map(improve).take(8)).head :: ...
// 1 :: improve(1) :: ...

import week2.Pouring

val problem = new Pouring(Vector(4, 9))
problem.moves
problem.solutions(6).head