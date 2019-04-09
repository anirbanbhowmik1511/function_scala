package parfuncational.parallel

import java.util.concurrent.TimeUnit

object ParApi {

  def main(args: Array[String]): Unit = {
    val l = List(List(1), List(2), List(), List(3))
    println(l.flatten)
  }

  import Par._

  type Par[A] = ExecutorService => Future[A]

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    Par.sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val as = a(es)
      val bs = b(es)
      UnitFuture(f(as.get, bs.get))
    }

    def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A]() {
      def call = a(es).get
    })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): A = a(s).get

    def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

    def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  }

  trait ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
  }

  trait Callable[A] { def call: A }

  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean

  }

  case class UnitFuture[A](get: A) extends Future[A] {
    def get(timeout: Long, unit: TimeUnit): A = get
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isDone: Boolean = true
    def isCancelled: Boolean = false
  }
}