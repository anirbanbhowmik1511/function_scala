package parfuncational.parallel

import java.util.concurrent.TimeUnit



class ParApi {
  import Par._
  
  type Par[A] = ExecutorService => Future[A]

  
  def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))
  
  object Par {
    def unit[A](a: A): Par[A] = (es : ExecutorService) => UnitFuture(a) 

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val as = a(es)
      val bs = b(es)
      UnitFuture(f(as.get, bs.get))
    }

    def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A](){
      def call = a(es).get
    })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): A = ???
  }
  
  trait ExecutorService {
    def submit[A](a : Callable[A]) : Future[A]
  }
  
  trait Callable[A]{def call : A}
  
  trait Future[A] {
    def get : A
    def get(timeout : Long, unit : TimeUnit) : A
    def cancel(evenIfRunning : Boolean) : Boolean
    def isDone : Boolean
    def isCancelled : Boolean
    
  }
  
  case class UnitFuture[A](get: A) extends Future[A] {
    def get(timeout : Long, unit : TimeUnit) : A = get
    def cancel(evenIfRunning : Boolean) : Boolean = false
    def isDone : Boolean = true
    def isCancelled : Boolean = false
  }
}