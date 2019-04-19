package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    k <- arbitrary[Int]
    m <- genHeap
  } yield insert(k, m))

  val twoElemHeap = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- insert(k, empty)
    if(k != v)
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwo") = forAll(twoElemHeap) { (h: H) =>
    val a = findMin(h)
    val m = deleteMin(h)
    val b = findMin(m)
    a < b
  }

  property("addToEmpty") = forAll(genHeap.suchThat(isEmpty), arbitrary[Int]) { (h: H, e: Int) =>
    val m = insert(e, h)
    isEmpty(deleteMin(m))
  }
  


  property("checkSorted") = forAll { (h: H) =>
    def isSorted(m: H, e: A): Boolean = {
      if (isEmpty(m)) true
      else if (e > findMin(m)) false
      else isSorted(deleteMin(m), findMin(m))
    }
    if (isEmpty(h)) true
    else isSorted(deleteMin(h), findMin(h))

  }

  property("checkMedling") = forAll { (h1: H, h2: H) =>
    val res = meld(h1, h2)
    if (isEmpty(h1)) h2 == res
    else if (isEmpty(h2)) h1 == res
    else findMin(h1) == findMin(res) || findMin(h2) == findMin(res)
  }
  
  property("setEquality") = forAll { (h1: H, h2 : H) =>
    val res = meld(h1, h2)
    def size(h : H, v : Set[Int]) : Set[Int]= {
      if(isEmpty(h)) v
      else size(deleteMin(h), v + findMin(h))
    }
    size(h1, Set()) ++ size(h2, Set()) == size(res, Set())
  }

}
