package frp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

object Test {
  def main(args : Array[String]) = {
    val a = Future(testFunction)
    val m = a.map(x => transform(x))
    val b = a.value
    while(!a.isCompleted){println(a.isCompleted)}
    val c= a.value
    println(b +" "+ c + " "+m.value)
  }
  
  def testFunction() : Int = { 
    println("Testing future")
    Thread.sleep(10)
    2
  }
  
  def transform(x : Int) = {
    println(x)
    throw new IllegalArgumentException
  }
}