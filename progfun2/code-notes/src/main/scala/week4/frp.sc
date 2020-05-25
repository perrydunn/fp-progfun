// Functional Reactive Programming
import week4.frp.{Signal, Var}

class BankAccount {
  val balance = Var(0)

  def currentBalance = balance

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    } else {
      throw new Error("Insufficient funds")
    }
}

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)

val a, b = new BankAccount()
val c = consolidated(List(a, b))

c()
a deposit 20
c()
b deposit 30
c()
a withdraw 10
c()
