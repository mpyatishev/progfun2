package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("deleteMin1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("insTwo") = forAll { (a1: Int, a2: Int) =>
   val h = insert(a1, insert(a2, empty))
   val min = a1.min(a2)
   findMin(h) == min
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
   val m1 = if (isEmpty(h1)) 0 else findMin(h1)
   val m2 = if (isEmpty(h2)) 0 else findMin(h2)
   val h = meld(h1, h2)
   val m = m1.min(m2)

   findMin(h) == m
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    def sorted(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: sorted(deleteMin(h))
    }

    val m1 = sorted(meld(h1, h2))
    val m2 = sorted(meld(h2, h1))
    m1 == m2
  }

  property("sorted") = forAll { h: H =>
    def sorted(h1: H): List[Int] = {
      if (isEmpty(h1)) Nil
      else findMin(h1) :: sorted(deleteMin(h1))
    }
    val xs = sorted(h)
    xs == xs.sorted
  }

  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def sorted(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: sorted(deleteMin(h))
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = sorted(meld1)
    val xs2 = sorted(meld2)
    xs1 == xs2
  }
}
