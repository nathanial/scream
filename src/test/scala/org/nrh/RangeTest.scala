package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.Range._
import org.scalatest._

object RangeTest {
  def main(args: Array[String]) {
    (new RangeTest).execute()
  }
}

class RangeTest extends Suite {

  def testEquality() {
    val a = range(0, 20)
    val b = range(0, 20)
    val c = range(2, 20)
    assert(Range.same(a,b))
    assert(Range.different(a,c))
    assert(Range.different(b,c))
  }

  def testIntersection() {
    val a = range(0, 100)
    val b = range(20)
    val c = (a intersect b).get
    assert(Range.same(c,range(20)))
  }

  def testUnion() {
    val a = range(20, 30)
    val b = range(30, 40)
    val c = a union b
    assert(Range.same(c,range(20,40)))
  }

}
