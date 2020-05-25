lazy val x: Stream[Int] = Stream.cons(1, x)
x.tail
x.tail.tail

def sqrtStream(x: Double): LazyList[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  val guesses: LazyList[Double] = 1 #:: (guesses map improve)
  guesses
}
