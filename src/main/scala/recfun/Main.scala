package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
    factorial(r) / (factorial(c) * factorial(r - c))
  }

  def balance(chars: List[Char]): Boolean = {
    def isOpen(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) true
      else if (chars.head == '(') isOpen(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && isOpen(chars.tail, open - 1)
      else isOpen(chars.tail, open)
    }
    isOpen(chars, 0)
  }


  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, length: Int): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (length <= 0 && money >= 1) 0
      else count(money, length - 1) + count(money - coins(length-1), length)
    }
    count(money, coins.length)
  }
}
