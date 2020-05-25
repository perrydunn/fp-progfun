package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)
    Signal(
      delta() match {
        case d if d < 0 => Set()
        case 0 => Set(-b() / (2 * a()))
        case d => {
          val denominator = 2 * a()
          val b2a = -b() / denominator
          val d2a = Math.sqrt(d) / denominator
          Set(b2a - d2a, b2a + d2a)
        }
      }
    )
  }
}
