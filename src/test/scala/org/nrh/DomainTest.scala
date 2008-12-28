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

class DomainTest extends Suite {

  def testEquality() {
    assert(domain(0 upto 100) == domain(0 upto 100))
    assert(domain(12) == domain(12))
    assert(domain(1 upto 2) != domain(3 upto 4))
    assert(domain(0 upto 100) != domain(2 upto 100))
    assert(domain(203 upto 400) != domain(0))
  }

  def testIntersection() {
    {
      val x = domain(0 upto 100) intersect domain(50 upto 75)
      println("Intersect1 = " + x)
      assert(x == domain(50 upto 75))
    }
    {
      val x = domain(0 upto 100) intersect domain(50 upto 150)
      println("Intersect2 = " + x)
      assert(x == domain(50 upto 100))
    }
    {
      val x = domain(0 upto 20, 40 upto 50)
      val y = domain(0 upto 20, 40 upto 60)
      val z = x intersect y
      println("Intersect3 = " + z)
      assert(z == domain(0 upto 20, 40 upto 50))
    }
    {
      val x = domain(0 upto 100)
      val y = domain(0 upto 25, 75 upto 100)
      val z = x intersect y
      println("Intersect4 = " + z)
      assert(z == domain(0 upto 25, 75 upto 100))
    }
    {
      val x = domain(0 upto 25, 50 upto 75, 100 upto 125)
      val y = domain(range(5), range(60), range(111))
      val z = x intersect y
      println("Intersect5 = " + z)
      assert(z == domain(range(5), range(60), range(111)))
    }
    {
      val x = domain(20)
      val y = domain(0 upto 10000)
      val z = x intersect y
      println("Intersect6 = " + z)
      assert(z == domain(20))
    }
    {
      val x = domain(20)
      val y = domain(13 upto 10013)
      val z = x intersect y
      println("Intersect7 = " + z)
      assert(z == domain(20))
    }
  }

  def testUnion() {
    {
      val a = domain(0 upto 30, 50 upto 60)
      val b = domain(0 upto 30, 40 upto 50)
      val c = a union b
      println("Union1 = " + c)
      assert(c == domain(0 upto 30, 40 upto 60))
    }
    {
      val a = domain(2) union domain(3)
      println("Union2 = " + a)
      assert(a == domain(2 upto 3))
    }
    {
      val a = domain(10 upto 20)
      val b = domain(30 upto 40)
      val c = a union b
      println("Union3 = " + c)
      assert(c == domain(10 upto 20, 30 upto 40))
    }
  }

}
    
    
