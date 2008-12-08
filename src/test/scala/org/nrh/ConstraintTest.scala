package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.scalatest._

object ConstraintTest {
  def main(args: Array[String]){
    (new ConstraintTest).execute()
  }
}

class ConstraintTest extends Suite {
  
  def testAdd(){ 
    {
      val p = new Problem
      val x = p.newVar
      val y = p.newVar
      p.constrain(
	x == 13,
	x + y == 20
      )
      val labels = p.label(Map("x" -> x, "y" -> y))
      assert(labels("x") === 13)
      assert(labels("y") === 7) 
    }
    {
      val p = new Problem
      val a = p.newVar
      val b = p.newVar
      val c = p.newVar
      p.constrain(
	a == 5,
	b == 4,
	a + b == c
      )
      val labels = p.label(Map("a" -> a, "b" -> b, "c" -> c))
      assert(labels("a") === 5) 
      assert(labels("b") === 4) 
      assert(labels("c") === 9) 
    }
    {
      val p = new Problem
      val a = p.newVar
      val b = p.newVar
      val c = p.newVar
      p.constrain(
	a == 17,
	b == 7,
	a - b == c
      )
      val labels = p.label(Map("a" -> a,"b" -> b,"c" -> c))
      assert(labels("a") === 17) 
      assert(labels("b") === 7) 
      assert(labels("c") === 10) 
    }
  }

  def testMultiply() {
    val p = new Problem
    val a = p.newVar
    val b = p.newVar
    val c = p.newVar
    p.constrain(
      a == 20,
      b == 12,
      a * b == c
    )
    val labels = p.label(Map("a" -> a, 
			     "b" -> b,
			     "c" -> c))
    assert(labels("a") === 20) 
    assert(labels("b") === 12) 
    assert(labels("c") === 240) 
  }

  def testDivide() {
    val p = new Problem
    val a = p.newVar
    val b = p.newVar
    val c = p.newVar
    p.constrain(
      a == 15,
      b == 3,
      a / b == c
    )
    val labels = p.label(Map("a" -> a,
			     "b" -> b,
			     "c" -> c))
    assert(labels("a") === 15) 
    assert(labels("b") === 3) 
    assert(labels("c") === 5) 
  }

  def testComplex() {
    val p = new Problem
    val a = p.newVar
    val b = p.newVar
    p.constrain(
      a + b == 20,
      a - b == 10
    )
    val labels = p.label(Map("a" -> a,
			     "b" -> b))
    assert(labels("a") === 15) 
    assert(labels("b") === 5) 
  }
}
