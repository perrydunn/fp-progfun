package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // genHeap, as written here, never produces an empty heap.
  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMin1: findMin on single element heap is that element") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // Catches Bogus 2
  property("findMin2: findMin of 2-elem heap returns min") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  // Catches Bogus 1
  property("findMin3: findMin on n-elem heap returns min") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMin1: deleteMin removes an element") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  // Catches Bogus 3
  property("deleteMin2: deleteMin removes actual min") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == Math.max(a, b)
  }

  // Catches Bogus 5
  property("isMinHeap: findMin deleteMin recursion produces sorted min-max") = forAll { h: H =>
    def isMinHeap(e: Int, h1: H): Boolean =
      if (isEmpty(h1)) true
      else if (e > findMin(h1)) false
      else isMinHeap(findMin(h1), deleteMin(h1))
    isMinHeap(findMin(h), deleteMin(h))
  }

  property("meld1: meld empty heaps produces empty heap") = forAll { _: Int =>
    isEmpty(meld(empty, empty))
  }

  property("meld2: meld findMin is correct") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  // Catches Bogus 4
  property("meld3: meld doesn't cheat - actually melds the whole of both heaps") =
    forAll { (h1: H, h2: H) =>
      def isEqual(h1: H, h2: H): Boolean =
        if (isEmpty(h1) && isEmpty(h2)) true
        else if (isEmpty(h1) || isEmpty(h2)) false
        else if (findMin(h1) == findMin(h2)) isEqual(deleteMin(h1), deleteMin(h2))
        else false
      isEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
    }

//  // This is a more manually written version of the above test
//  property("meld3") = forAll { (a: Int, b: Int, c: Int) =>
//    val h1 = meld(insert(a, insert(b, empty)), insert(c, empty))
//    val h2 = meld(insert(a, empty), insert(b, insert(c, empty)))
//    val m = List(a, b, c).max
//    findMin(deleteMin(deleteMin(h1))) == m &&
//      findMin(deleteMin(deleteMin(h2))) == m
//  }
}
