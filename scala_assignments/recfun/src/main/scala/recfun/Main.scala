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
      if(c == 0 || c == r) 1
      else pascal(c, r-1) + pascal(c - 1, r - 1)        
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def bal(x : Int, tail : List[Char]) : Boolean = tail match {
        case List() => x == 0
        case '(' :: tl => bal(x + 1, tl)
        case ')' :: tl => if(x == 0) false else bal(x - 1, tl)
        case _ :: tl => bal(x, tl)
      }
      bal(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = coins match {
        case List() => if(money == 0) 1 else 0
        case x :: tail => if(x > money) countChange(money, tail) else countChange(money, tail) + countChange(money - x, coins) 
    }
  }
