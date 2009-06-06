package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.DomainException
import org.scalatest._
import org.nrh.scream.Interval._
import org.nrh.scream.IntervalImplicits._
import scala.util.Sorting

object DomainTest {
  def main(args: Array[String]) {
    (new DomainTest).execute()
  }
}

class DomainTest extends Suite with Logging {

  def testEquality1() {
    assert(domain(0 upto 100) == domain(0 upto 100))
    assert(domain(12) == domain(12))
    assert(domain(1 upto 2) != domain(3 upto 4))
    assert(domain(0 upto 100) != domain(2 upto 100))
    assert(domain(203 upto 400) != domain(0))
  }

  def testEquality2() {
    val a = domain(15)
    val b = domain(15 upto 20)
    logger.info("Equality1 = " + a + " " + b)
    assert((a == b) == false)
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
    val y = domain(interval(5), interval(60), interval(111))
    val z = x intersect y
    logger.info("Intersect5 = " + z)
    assert(z == domain(interval(5), interval(60), interval(111)))
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

  def testIntersection10(){
    val x = domain(0 upto 10)
    val y = domain(11 upto 20)
    val z = x intersect y
    logger.info("Intersect10 = " + z)
    assert(z == EmptyDomain)
  }

  def testIntersection11() {
    val x = domain(2 upto 9)
    val y = domain(0 upto 0, 2 upto 9)
    val z = x intersect y
    val z2 = y intersect x
    logger.info("Intersect11 = " + z)
    assert(z == domain(2 upto 9))
    assert(z2 == domain(2 upto 9))
    assert(z == z2)
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

  def testRemove1() {
    val a = domain(0 upto 10)
    val b = domain(0 upto 5)
    val c = a remove b
    logger.info("DomainRemove1 = " + c)
    assert(c == domain(6 upto 10))
  }

  def testRemove2() {
    val a = domain(0 upto 10, 15 upto 20)
    val b = domain(0 upto 5, 17 upto 20)
    val c = a remove b
    logger.info("DomainRemove2 = " + c)
    assert(c == domain(6 upto 10, 15 upto 16))
  }

  def testRemove3() {
    val a = domain(0 upto 10, 20 upto 30)
    val b = domain(11 upto 19)
    val c = a remove b
    logger.info("DomainRemove3 = " + c)
    assert(c == domain(0 upto 10, 20 upto 30))
  }

  def testRemove4() {
    val a = domain(0 upto 10)
    val b = domain(0 upto 10)
    val c = a remove b
    logger.info("DomainRemove4 = " + c)
    assert(c == EmptyDomain)
  }

  def testRemove5() {
    val a = domain(0 upto 0, 2 upto 9)
    val b = domain(0 upto 0)
    val c = a remove b
    logger.info("DomainRemove5 = " + c)
    assert(c == domain(2 upto 9))
  }

  def testLength1() {
    val a = domain(0 upto 10)
    val b = domain(0 upto 0)
    logger.info("Domain Length1")
    assert(a.length > b.length)
  }

  def testLength2() {
    val a = domain(100 upto 100)
    val b = domain(0 upto 10)
    logger.info("Domain Length2")
    assert(a.length < b.length)
  }

  def testLength3() {
    val a = domain(100 upto 100)
    val b = domain(0 upto 0)
    logger.info("Domain Length3")
    assert(a.length == b.length)
  }

  def testLength4() {
    val a = domain(0 upto 0, 2 upto 2, 4 upto 4)
    val b = domain(0 upto 2)
    logger.info("Domain Length4")
    logger.info("a.length = " + a.length)
    logger.info("b.length = " + b.length)
    assert(a.length == b.length)
  }

  def testOrdering1() {
    val (a,b,c,d) = (domain(0 upto 50),domain(0 upto 2),
		     domain(0 upto 0),domain(0 upto 100))
    val list:List[Domain] = Sorting.stableSort(a :: b :: c :: d :: Nil).toList
    logger.info("Domain Ordering1 " + list)
    assert(list == (c :: b :: a :: d :: Nil))
  }

  def testOrdering2() {
    val (a,b,c) = (domain(0 upto 2, 4 upto 12), 
		   domain(50 upto 80), domain(0 upto 10))
    val list:List[Domain] = Sorting.stableSort(a :: b :: c :: Nil).toList
    logger.info("Domain Ordering2 " + list)
    assert(list == (c :: a :: b :: Nil))
  }
      
  def testMinMax1() {
    val a = domain(10 upto 105)
    assert(a.min == 10)
    assert(a.max == 105)
  }

  def testMinMax2() {
    val a = domain(0 upto 5, 17 upto 200, 8 upto 9)
    assert(a.min == 0)
    assert(a.max == 200)
  }

  def testToBigInt1() {
    val a = singleton(2)
    assert(a.toBigInt == 2)
  }

  def testToBigInt2() {
    val a = domain(1 upto 2)
    try {
      a.toBigInt
      fail()
    } catch {
      case (e: DomainException) => {}
    }
  }
    
  def testAddition1() {
    val a = domain(12 upto 15)
    val b = domain(0 upto 2)
    val c = a + b
    assert(c == domain(12 upto 17))
  }

  def testAddition2() {
    val a = singleton(10)
    val b = singleton(20)
    val c = a + b
    assert(c == singleton(30))
  }

  def testSubtraction1() {
    val a = domain(20 upto 30)
    val b = domain(5 upto 10)
    val c = a - b
    assert(c == domain(10 upto 25))
  }

  def testSubtraction2() {
    val a = domain(10 upto 12)
    val b = domain(20 upto 30)
    val c = a - b
    assert(c == domain(-20 upto -8))
  }

  def testMultiplication1() {
    val a = domain(22 upto 33)
    val b = domain(0 upto 3)
    val c = a * b
    assert(c == domain(0 upto 99))
  }

  def testMultiplication2() {
    val a = singleton(2)
    val b = singleton(25)
    val c = a * b
    assert(c == domain(50))
  }

  def testDivision1() {
    val a = domain(2 upto 3, 55 upto 100)
    val b = singleton(5)
    val c = a / b
    val d = b / a
    assert(c == domain(0 upto 20))
    assert(d == domain(0 upto 2))
  }    

  def testOverlap1() {
    val a = domain(0 upto 20)
    val b = domain(-20 upto 0)
    assert(a overlap b)
  }

  def testOverlap2() {
    val a = singleton(7)
    val b = singleton(8)
    assert(!(a overlap b))
  }

  def testContains1() {
    val a = domain(2 upto 8, 10 upto 20)
    assert(a contains 7)
    assert(a contains 8)
    assert(!(a contains 9))
    assert(a contains 15)
    assert(a contains 10)
    assert(a contains 20)
    assert(!(a contains 0))
  }

  def testSubset1() {
    val a = domain(0 upto 20)
    val b = singleton(5)
    assert(b subset a)
    assert(!(a subset b))
  }

  def testIsSingleton1() {
    val a = singleton(4)
    val b = domain(0 upto 2)
    val c = domain(0 upto 0)

    assert(a.isSingleton)
    assert(!b.isSingleton)
    assert(c.isSingleton)
  }

  def testElements1() {
    val a = domain(0 upto 5, 10 upto 15)
    val els = a.elements.toList
    assert(els == (0 :: 1 :: 2 :: 3 :: 4 :: 5 ::
		   10 :: 11 :: 12 :: 13 :: 14 :: 15 :: Nil))
    
  }
  
  def testRandomizedElements1() {
    val a = domain(0 upto 5)
    val els = a.randomizedElements.toList
    logger.info("randomized elements = " + els)
    logger.info("randomized elements2 = " + a.randomizedElements.toList)
    logger.info("randomized elements3 = " + a.randomizedElements.toList)
    assert(els != (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))
    assert(els.contains(0) &&
	   els.contains(1) &&
	   els.contains(2) && 
	   els.contains(3) && 
	   els.contains(4) &&
	   els.contains(5))
  }

}
