package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b: Int) => 
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val lower = if (a < b) a else b
    findMin(h2) == lower
  }
  
  property("delete1") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }
  
  property("delete2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    isEmpty(deleteMin(deleteMin(h)))
  }
  
  property("heap1") = forAll { a: H =>
    def checkOrdering(x: H): Boolean = {
      if (isEmpty(x)) {
        true
      } else {
        val truncated = deleteMin(x)
        if (isEmpty(truncated)) {
          true
        } else {
          if (ord.compare(findMin(x), findMin(truncated)) <= 0) {
            checkOrdering(truncated)
          } else {
            false
          }
        }
      }
    }
    checkOrdering(a)
  }
  
  property("heap2") = forAll { (a: Int, b: Int, c: Int) =>
    def checkOrdering(x: H): Boolean = {
      if (isEmpty(x)) {
        true
      } else {
        val truncated = deleteMin(x)
        if (isEmpty(truncated)) {
          true
        } else {
          if (ord.lteq(findMin(x), findMin(truncated))) {
            checkOrdering(truncated)
          } else {
            false
          }
        }
      }
    }
  	val h = insert(a, insert(b, insert(c, empty)))
  	checkOrdering(h)
  }
  
  property("meld1") = forAll { (a: H, b: H) =>    
    val h = meld(a, b)
    
    val aMin = findMin(a)
    val bMin = findMin(b)
    val lower = if (aMin < bMin) aMin else bMin
    
    findMin(h) == lower
  }
  
  property("meld2") = forAll { (a: H, b: H) =>
    val h1 = deleteMin(a)
    val h2 = insert(findMin(a), b)
    
    def flatten(x: H, previous: List[Int]): List[Int] = { 
      if (isEmpty(x)) previous else flatten(deleteMin(x), findMin(x) :: previous)
    }
    flatten(meld(a, b), List[Int]()).sorted == flatten(meld(h1, h2), List[Int]()).sorted
  }
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(k, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
