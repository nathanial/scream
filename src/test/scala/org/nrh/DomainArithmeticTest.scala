package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.DomainArithmetic._
import org.scalatest._

object DomainArithmeticTest {
  def main(args: Array[String]) {
    (new DomainArithmeticTest).execute()
  }
}

class DomainArithmeticTest extends Suite {

  def testAddition() {
    {
      val a = domain(10, 20)
      val b = domain(22, 34)
      val c = a + b
      assert(c === domain(32,54)) 
    }
    {
      val a = domain(0, 100)
      val b = domain(0)
      val c = a + b
      assert(c === a)
    }
  }
  
  def testSubtraction() {
    val a = domain(20, 40)
    val b = domain(10, 25)
    val c = a - b
    assert(c === domain(-5, 30))
  }

}
