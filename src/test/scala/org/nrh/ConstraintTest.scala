package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.Debug._
import org.nrh.scream.Range._
import org.scalatest._

object ConstraintTest {
  def main(args: Array[String]){
    (new ConstraintTest).execute()
  }
}

class ConstraintTest extends Suite {

  def assertSame(d1:Domain, d2:Domain){
    assert(d1 == d2)
  }
  
  def testAdd(){ 
    {
      debug("\n")
      debug("Begin testADD1")
      val p = new Problem
      val x = p.newVar("x")
      val y = p.newVar("y")
      x := 13
      x + y := 20
   
      debug("\n")
      debug("Solving testADD1")
      p.solve
      
      assertSame(p("x"),13)
      assertSame(p("y"),7)

      debug("Finished testADD1")
      debug("\n")
    }
    {
      debug("\n")
      debug("Begin testADD2")
      val p = new Problem
      val a = p.newVar("a")
      val b = p.newVar("b")
      val c = p.newVar("c")
      a := 5
      b := 4
      a + b == c

      debug("\n")
      debug("Solving testADD2")
      p.solve

      assertSame(p("a"),5)
      assertSame(p("b"),4)
      assertSame(p("c"),9)

      debug("Finished testADD2")
      debug("\n")
    }
    {
      debug("\n")
      debug("Begin test-add3")
      val p = new Problem
      val a = p.newVar("a")
      val b = p.newVar("b")
      val c = p.newVar("c")
      a := 17
      b := 7
      a - b == c
      
      debug("\n")
      debug("Solving test-add3")
      p.solve
      
      assertSame(p("a"),17)
      assertSame(p("b"),7)
      assertSame(p("c"),10)

      debug("Finished test-add3")
      debug("\n")
    }
  }
  def testMultiply() {
    debug("\n")
    debug("Begin test mult1")
    val p = new Problem
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")
    a := 20
    b := 12
    a * b == c

    debug("\n")
    debug("Solving test mult1")
    p.solve

    assertSame(p("a"),20)
    assertSame(p("b"),12)
    assertSame(p("c"),240)

    debug("Finished test mult3")
    debug("\n")
  }

  def testDivide() {
    debug("\n")
    debug("Begin test div1")
    val p = new Problem
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")
    a := 15
    b := 3
    a / b == c

    debug("\n")
    debug("Solving test div1")
    p.solve
    
    assertSame(p("a"), 15)
    assertSame(p("b"), 3)
    assertSame(p("c"), 5)

    debug("Finished test div1")
    debug("\n")
  }

  def testComplex() {
    debug("\n")
    debug("Begin test complex1")
    val p = new Problem
    val a = p.newVar("a")
    val b = p.newVar("b")
    a + b := 20
    a - b := 10

    debug("\n")
    debug("Solving test complex1")
    p.solve

    debug("a = " + p("a"))
    debug("b = " + p("b"))

    assertSame(p("a"), domain(10 upto 20))
    assertSame(p("b"), domain(0 upto 10))

    debug("Finished test complex1")
    debug("\n")
  }
}
