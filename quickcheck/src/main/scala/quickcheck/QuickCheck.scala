package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //lazy val genHeap: Gen[H] = ???
  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(element, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // A generator for maps of type Map[Int,Int]
  lazy val genMap: Gen[Map[Int, Int]] = oneOf(
    const(Map.empty[Int, Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int, Int]), genMap)
    } yield m.updated(k, v)
  )

  // a generator of any random heap
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // if you insert an element into an empty heap, then find the minimum of the resulting heap
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting
  // heap should get the smallest of the two elements back.
  property("insertTwoElementsIntoEmptyHeapFindingMin") = forAll { (a: A, b: A) =>
    val heap = insert(a, insert(b, empty))
    findMin(heap) == (a min b)
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insertElementIntoEmptyHeapDeleteMin") = forAll { (a: A, b: A) =>
    insert(a min b, empty) == deleteMin(insert(a, insert(b, empty)))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimal.
  property("sortedFindDel") = forAll { heap: H =>
    def loopFindDelMin(heap: H): List[A] = heap match {
      case h if isEmpty(h) => List.empty
      case _ => findMin(heap) :: loopFindDelMin(deleteMin(heap))
    }

    def isSorted[T](l: Iterable[T])(implicit ord: Ordering[T]): Boolean = l.sliding(2).forall { case List(x, y) => ord.lt(x, y) }

    isSorted(loopFindDelMin(heap))

  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("findingMinMeldTwoHeap") = forAll { (heap1: H, heap2: H) =>
    (findMin(heap1) min findMin(heap2)) == findMin(heap1)

  }
}
