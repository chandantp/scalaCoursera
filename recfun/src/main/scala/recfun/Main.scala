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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val LBRACE = '('
    val RBRACE = ')'
    var balanced = true

    def ibal(chars: List[Char], stack: collection.mutable.Stack[Char]): (Boolean, List[Char]) = {
      var tmp = chars

      while(balanced && !tmp.isEmpty) {
        if (tmp.head == LBRACE) {
          val res = ibal(tmp.tail, stack.push(LBRACE))
          balanced = res._1
          tmp = res._2
        }
        else if (tmp.head == RBRACE) {
          val isBalanced = !stack.isEmpty
          if (!stack.isEmpty) stack.pop
          return (isBalanced, tmp.tail)
        }
        else {
          tmp = tmp.tail
        }
      }
      (balanced, tmp)
    }

    val stack = collection.mutable.Stack[Char]()
    val res = ibal(chars, stack)
    stack.isEmpty && res._1
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange2(money: Int, coins: List[Int], change: String): Int = {
      if (money < 0) 0
      else if (money == 0) 1
      else {
        var count = 0
        for(coin <- coins) {
          val lcoin = change.split(":").last.toInt
          if (coin >= lcoin) {
            val newchange = change + ":" + coin
            count += countChange2(money - coin, coins, newchange)
          }
        }
        count
      }
    }
    countChange2(money, coins, "0")
  }
}
