package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.scalatest._

object VarTest {
  def main(args: Array[String]) {
    (new VarTest).execute()
  }
}

class VarTest extends Suite {

  def testAddition() {
    {
      val p = new Problem
      val x = p.newVar(domain(3,13))
      val y = p.newVar(domain(1,20))
      val a = new AdditionVar(x,y)
      assert(a.domain === domain(4,33)) 
    }
    {
      val p = new Problem
      val x = p.newVar(domain(3,13))
      val y = p.newVar(domain(1,20))
      val z = p.newVar(domain(5))
      val a = new AdditionVar(x,y)
      val e = new EqualityVar(a,z)
      assert(e.domain === domain(5)) 
      assert(a.domain === domain(5)) 
      assert(x.domain === domain(3,4)) 
      assert(y.domain === domain(1,2)) 
    }
  }

  def testAdditionSubtraction() {
    val p = new Problem

    val x = p.newVar(domain(0,20))
    val y = p.newVar(domain(0,20))

    val add = new AdditionVar(x,y)
    val e1 = new EqualityVar(add, p.newVar(domain(20)))

    val sub = new SubtractionVar(x,y)
    val e2 = new EqualityVar(sub, p.newVar(domain(10)))

    assert(x.domain === 15) 
    assert(y.domain === 5) 
    assert(e1.domain === 20) 
    assert(e2.domain === 10) 
    assert(add.domain === 20) 
    assert(sub.domain === 15) 
  }
}
    
    
