package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def bal(x : Int, tail : List[Char]) : Boolean = tail match {
        case List() => x == 0
        case '(' :: tl => bal(x + 1, tl)
        case ')' :: tl => if(x == 0) false else bal(x - 1, tl)
        case _ :: tl => bal(x, tl)
      }
      bal(0, chars.toList)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @annotation.tailrec
    def traverse(idx: Int, until: Int, open: Int, close: Int) : (Int, Int) = {
      if(idx == until) (open, close)
      else if (chars(idx) == ')'){
        if(open == 0) traverse(idx + 1, until, open, close + 1)
        else traverse(idx + 1, until, open - 1, close)
      }
      else if (chars(idx) == '(') traverse(idx + 1, until, open + 1, close)
      else traverse(idx + 1, until, open, close)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + ((until - from) / 2)
        val ((x1,y1), (x2,y2)) = parallel(reduce(from, mid), reduce(mid, until)) 
        
        val lres = if(x1 >= y2) x2 + (x1 - y2) else x2
        val rres = if(x1 < y2) y1 + (y2 - x1) else y1         
        (lres, rres)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
