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
    assert(Domain.same(
      domain(0,100),
      domain(0,100)))
    assert(Domain.same(
      domain(12),
      domain(12)))
    assert(Domain.different(
      domain(1,2),
      domain(3,4)))
    assert(Domain.different(
      domain(0,100),
      domain(2,100)))
    assert(Domain.different(
      domain(203,400),
      domain(0)))
  }

  def testIntersection() {
    val x1 = domain(0, 100) intersect domain(50, 75)
    assert(Domain.same(x1, domain(50,75)))

    val x2 = domain(0,100) intersect domain(50,150)
    assert(Domain.same(x2, domain(50,100)))
  }

  def testUnion() {
    val c = domain(2) union domain(3)
    assert(Domain.same(c, domain(2,3)))
  }
}
    
    
