package recfun

import scala.annotation.tailrec

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
  def pascal(column: Int, row: Int): Int = {
    if (column == 0 || column == row) 1
    else pascal(column - 1, row - 1) + pascal(column, row - 1)
  }

  /**
   *
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def occurrences(occurs: Int, chars: List[Char]): Boolean = {
      if (occurs < 0){
        false
      } else if(chars.isEmpty && occurs > 0) {
        false
      } else {
        chars match {
          case Nil => true
          case x :: tail => {
            if (x == ')') {
              occurrences(occurs - 1, tail)
            }
            else {
              if (x == '(')
                occurrences(occurs + 1, tail)
              else
                occurrences(occurs, tail)
            }
          }
        }
      }
    }

    occurrences(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else
    if (money == 0) 1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
