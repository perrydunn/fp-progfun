// The Observer Pattern (Imperative Event Handling)
// Deprecated
import week4.ieh.{Publisher, Subscriber}

class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance = balance

  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
    publish()
  }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      publish()
    } else {
      throw new Error("Insufficient funds")
    }
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  // underscore means uninitialised
  private var total: Int = _
  compute()

  // Most straightforward and simple, though not efficient
  private def compute() =
    total = observed.map(_.currentBalance).sum

  def handler(pub: Publisher) = compute()

  def totalBalance = total
}

val a, b = new BankAccount
val c = new Consolidator(List(a, b))
c.totalBalance

a deposit 20
c.totalBalance
b deposit 30
c.totalBalance
