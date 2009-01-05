package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._
import org.nrh.scream.IntervalImplicits._
import org.scalatest._

object IntervalTest {
  def main(args: Array[String]) {
    (new IntervalTest).execute()
  }
}

class IntervalTest extends Suite with Logging {

  def testEquality1() {
    val a = interval(0, 20)
    val b = interval(0, 20)
    val c = interval(2, 20)
    assert(a == b)
    assert(a != c)
    assert(b != c)
  }

  def testEquality2() { 
    val a = interval(0,20)
    val b = interval(0,20)
    val c = interval(2,20)
    assert(a == b)
    assert(b == a)
    assert(a != c)
    assert(b != c)
  }

  def testIntersection1() {
    val a = interval(0, 100)
    val b = interval(20)
    val c = (a intersect b)
    assert(c == interval(20))
  }

  def testUnion1() {
    val a = interval(20, 30)
    val b = interval(30, 40)
    val c = a union b
    assert(c == interval(20,40))
  }

  def testUnion2() {
    try {
      val a = interval(34, 233)
      val b = interval(11, 22)
      val c = a union b
      fail()
    } catch {
      case (e:IntervalException) => {}
    }
  }

  def testContract1() {
    try{
      val a = 20 upto -20
      fail()
    } catch {
      case (e:IntervalException) => {}
    }
  }

  def testIteration1() {
    val a = 0 upto 10
    var sum:BigInt = 0
    for(n <- a){      
      sum += n
    }
    logger.info("Iteration1 = " + sum)
    assert(sum == 55)
  }   

  def testRemove1() {
    val a = 0 upto 10
    val b = 5 upto 5
    val c = a remove b
    logger.info("Remove1 = " + c)
    assert(c == (0 upto 4) :: (6 upto 10) :: Nil)
  }

  def testRemove2() {
    val a = 0 upto 10
    val b = 0 upto 5
    val c = a remove b
    logger.info("Remove2 = " + c)
    assert(c == (6 upto 10) :: Nil)
  }

  def testRemove3() {
    val a = 0 upto 10
    val b = 5 upto 10
    val c = a remove b
    logger.info("Remove3 = " + c)
    assert(c == (0 upto 4) :: Nil)
  }

  def testRemove4() {
    val a = 5 upto 5
    val b = 0 upto 10
    val c = a remove b
    logger.info("Remove4 = " + c)
    assert(c == Nil)
  }

  def testRemove5() {
    val a = 0 upto 10
    val b = 11 upto 20
    val c = a remove b
    logger.info("Remove5 = " + c)
    assert(c == a :: Nil)
  }
}

