class BankAccount {
  private var balance = 0

  def deposit(amount: Int): Unit =
    if (amount > 0) balance = balance + amount

  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else {
      throw new Error("Insufficient funds")
    }
}

val account = new BankAccount
account deposit 100
account withdraw 10
// account withdraw 100

// Discrete Event Simulation
import week3._

object sim extends Circuits with Parameters
import sim._

val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)

in1 setSignal true
run()

in2 setSignal true
run()

in1 setSignal false
run()
