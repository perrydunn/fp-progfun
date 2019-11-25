package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || c == r || r == 0) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def counter(x: Char, xs: List[Char], count: Int): Int = {
        def parenthesis: Int =
          if (x == '(') 1 else if (x == ')') -1 else 0
        if (x == ')' && count == 0) {
          -1
        } else if (xs.isEmpty) {
          count + parenthesis
        } else {
          counter(xs.head, xs.tail, count + parenthesis)
        }
      }
      if (chars.isEmpty) {
        true
      } else if (counter(chars.head, chars.tail, 0) == 0) {
        true
      } else false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def safeCountChange(safeMoney: Int, safeCoins: List[Int]): Int = 
        if (safeCoins.isEmpty || safeMoney < 0) {
          0
        } else if (safeMoney == 0) {
          1
        } else {
          safeCountChange(safeMoney - safeCoins.head, safeCoins) +
          safeCountChange(safeMoney, safeCoins.tail)
        }
      if(money == 0 || coins.isEmpty) 0 else safeCountChange(money, coins)
    }
  }
