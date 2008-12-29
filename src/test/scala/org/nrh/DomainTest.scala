package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.scalatest._
import org.nrh.scream.Range._

object DomainTest {
  def main(args: Array[String]) {
    (new DomainTest).execute()
  }
}

class DomainTest extends Suite with Logging {

  def testEquality() {
    assert(domain(0 upto 100) == domain(0 upto 100))
    assert(domain(12) == domain(12))
    assert(domain(1 upto 2) != domain(3 upto 4))
    assert(domain(0 upto 100) != domain(2 upto 100))
    assert(domain(203 upto 400) != domain(0))
  }

  def testIntersection() {
    val x = domain(0 upto 100) intersect domain(50 upto 75)
    logger.info("Intersect1 = " + x)
    assert(x == domain(50 upto 75))
  }

  def testIntersection2() {
    val x = domain(0 upto 100) intersect domain(50 upto 150)
    logger.info("Intersect2 = " + x)
    assert(x == domain(50 upto 100))
  }

  def testIntersection3(){
    val x = domain(0 upto 20, 40 upto 50)
    val y = domain(0 upto 20, 40 upto 60)
    val z = x intersect y
    logger.info("Intersect3 = " + z)
    assert(z == domain(0 upto 20, 40 upto 50))
  }

  def testIntersection4(){
    val x = domain(0 upto 100)
    val y = domain(0 upto 25, 75 upto 100)
    val z = x intersect y
    logger.info("Intersect4 = " + z)
    assert(z == domain(0 upto 25, 75 upto 100))
  }

  def testIntersection5(){
    val x = domain(0 upto 25, 50 upto 75, 100 upto 125)
    val y = domain(range(5), range(60), range(111))
    val z = x intersect y
    logger.info("Intersect5 = " + z)
    assert(z == domain(range(5), range(60), range(111)))
  }

  def testIntersection6()  {
    val x = domain(20)
    val y = domain(0 upto 10000)
    val z = x intersect y
    logger.info("Intersect6 = " + z)
    assert(z == domain(20))
  }

  def testIntersection7(){
    val x = domain(20)
    val y = domain(13 upto 10013)
    val z = x intersect y
    logger.info("Intersect7 = " + z)
    assert(z == domain(20))
  }

  def testIntersection8(){
    val x = domain(0 upto 9, 15 upto 20)
    val y = domain(0 upto 20)
    val z = x intersect y
    logger.info("Intersect8 = " + z)
    assert(z == domain(0 upto 9, 15 upto 20))
  }

  def testIntersection9(){
    val x = domain(0 upto 9, 15 upto 20)
    val y = domain(10 upto 20)
    val z = x intersect y
    logger.info("Intersect9 = " + z)
    assert(z == domain(15 upto 20))
  }

  def testUnion() {
    val a = domain(0 upto 30, 50 upto 60)
    val b = domain(0 upto 30, 40 upto 50)
    val c = a union b
    logger.info("Union1 = " + c)
    assert(c == domain(0 upto 30, 40 upto 60))
  }

  def testUnion2() {
    val a = domain(2) union domain(3)
    logger.info("Union2 = " + a)
    assert(a == domain(2 upto 3))
  }

  def testUnion3() {
    val a = domain(10 upto 20)
    val b = domain(30 upto 40)
    val c = a union b
    logger.info("Union3 = " + c)
    assert(c == domain(10 upto 20, 30 upto 40))
  }

  def testEquality1() {
    val a = domain(15)
    val b = domain(15 upto 20)
    logger.info("Equality1 = " + a + " " + b)
    assert((a == b) == false)
  }
}
    
    
