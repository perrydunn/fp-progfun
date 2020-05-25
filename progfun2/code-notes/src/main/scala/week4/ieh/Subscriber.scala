package week4.ieh

trait Subscriber {
  def handler(pub: Publisher): Unit
}
