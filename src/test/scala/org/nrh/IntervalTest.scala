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

}
