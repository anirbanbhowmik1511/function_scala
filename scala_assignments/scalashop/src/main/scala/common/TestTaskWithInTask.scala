package common

object TestTaskWithInTask {
  def main(args : Array[String]) : Unit = {
    val t1 = task {
      println("Thread Name = "+ Thread.currentThread())
      
      val t3 = task {
        println("task with in task Thread Name = "+ Thread.currentThread())
        Thread.sleep(1000)
      }
      Thread.sleep(3000)
    }
    
    val t2 = task {
      println("Thread Name = "+ Thread.currentThread())
      Thread.sleep(1000)
    }
    
    val t3 = task {
      println("Thread Name = "+ Thread.currentThread())
      Thread.sleep(1000)
    }
    
    
    t1.join()
    t2.join()
    t3.join()
  }
}