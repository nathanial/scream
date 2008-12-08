package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.scalatest._

object DomainTest {
  def main(args: Array[String]) {
    (new DomainTest).execute()
  }
}

class DomainTest extends Suite {

  def testEquality() {
    assert(domain(0,100) === domain(0,100))
    assert(domain(12) === domain(12)) 
    assert(domain(1,2) != domain(3,4), "domain(1,2) != domain(3,4) false")
    assert(domain(0,100) != domain(2,100), "domain(0,100) != domain(2,100)")
    assert(domain(203,400) != domain(0), "domain(203,400) != domain(0)") 
  }

  def testIntersection() {
    val x1 = domain(0, 100) intersection domain(50, 75)
    assert(x1 === domain(50,75)) 

    val x2 = domain(0,100) intersection domain(50,150)
    assert(x2 === domain(50,100)) 
  }

  def testUnion() {
    val c = domain(2) union domain(3)
    assert(c === domain(2,3)) 
  }
}
    
    
