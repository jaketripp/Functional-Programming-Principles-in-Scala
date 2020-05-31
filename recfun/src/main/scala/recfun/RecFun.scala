package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(r: Int, c: Int): Int = {
    // tip of triangle or left+right boarders
    if (c == 0 || c == r) 1
    // normal case
    else pascal(r-1, c-1) + pascal(r-1, c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def countLoneParens(leftParenAcc: Int, chars: List[Char]): Int = {
      // out of characters to check, make sure we don't have any remaining left parens
      if (chars.isEmpty) leftParenAcc

      // found a left
      else if (chars.head == '(') countLoneParens(leftParenAcc+1, chars.tail)

      // found a right
      else if (chars.head == ')') {

        // it was a match
        if (leftParenAcc > 0) countLoneParens(leftParenAcc-1, chars.tail)

        // it's unbalanced
        else -1
      }

      // garbage character
      else countLoneParens(leftParenAcc, chars.tail)
    }

    countLoneParens(0, chars.toList) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
