package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.DomainImplicits._
import org.nrh.scream.Interval._
import org.nrh.scream.IntervalImplicits._
import org.scalatest._

object ConstraintTest {
  def main(args: Array[String]){
    (new ConstraintTest).execute()
  }
}

class ConstraintTest extends Suite with Logging {

  def assertSame(d1:Domain, d2:Domain){
    assert(d1 == d2)
  }
  
  def testAdd1(){  
    logger.info("Begin testADD1")
    val p = new Problem
    implicit val state = p.state
    val x = p.newVar("x")
    val y = p.newVar("y")
    
    x := 13
    x + y := 20
    
    logger.info("Solving testADD1")
    p.propogateConstraints
    logger.info("x = " + x.domain)
    logger.info("y = " + y.domain)

    assertSame(x.domain,13)
    assertSame(y.domain,7)

    logger.info("Finished testADD1")
  }

  def testAdd2(){
    logger.info("Begin testADD2")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")

    a := 5
    b := 4
    a + b == c
    
    logger.info("Solving testADD2")
    p.propogateConstraints

    assertSame(a.domain,5)
    assertSame(b.domain,4)
    assertSame(c.domain,9)

    logger.info("Finished testADD2")
  }
  def testAdd3(){
    logger.info("Begin test-add3")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")

    a := 17
    b := 7
    a - b == c
    
    logger.info("Solving test-add3")
    p.propogateConstraints
    
    assertSame(a.domain,17)
    assertSame(b.domain,7)
    assertSame(c.domain,10)

    logger.info("Finished test-add3")
    
  }
  def testMultiply() {
    logger.info("Begin test mult1")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")

    a := 20
    b := 12
    a * b == c
    
    logger.info("Solving test mult1")
    p.propogateConstraints

    assertSame(a.domain,20)
    assertSame(b.domain,12)
    assertSame(c.domain,240)

    logger.info("Finished test mult3")
    
  }

  def testDivide() {
    logger.info("Begin test div1")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")

    a := 15
    b := 3
    a / b == c

    logger.info("Solving test div1")
    p.propogateConstraints
    
    assertSame(a.domain, 15)
    assertSame(b.domain, 3)
    assertSame(c.domain, 5)

    logger.info("Finished test div1")
  }

  def testComplex() {
    logger.info("Begin test complex1")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")

    a + b := 20
    a - b := 10

    logger.info("Solving test complex1")
    p.propogateConstraints

    logger.info("a = " + a)
    logger.info("b = " + b)

    assertSame(a.domain, domain(10 upto 20))
    assertSame(b.domain, domain(0 upto 10))

    logger.info("Finished test complex1")
  }

  def testComplex2() {
    logger.info("Begin test complex2")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a", domain(0 upto 9, 15 upto 20))
    val b = p.newVar("b")

    a + b := 20
    a - b := 10
    
    logger.info("Solving test complex2")
    p.propogateConstraints

    logger.info("a = " + a)
    logger.info("b = " + b)

    assertSame(a.domain, 15)
    assertSame(b.domain, 5)
    
    logger.info("Finished test complex2")
  }

  def testComplex3() {
    logger.info("begin test complex3")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a", domain(0 upto 9, 15 upto 20))
    val b = p.newVar("b")
    val c = p.newVar("c")

    b := 2
    a + b == c
    
    logger.info("Solving test complex3")
    p.propogateConstraints
    
    logger.info("a = " + a)
    logger.info("b = " + b)
    logger.info("c = " + c)

    assertSame(a.domain, domain(0 upto 9, 15 upto 20))
    assertSame(b.domain, 2)
    assertSame(c.domain, domain(2 upto 22))

    logger.info("Finished test complex3")
  }

  def testComplex4() {
    logger.info("begin test complex4")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    
    a + b := 20
    a - b := 10
    
    logger.info("Propogating Constraints complex4")
    p.propogateConstraints
    state = p.findSolution
    
    logger.info("a = " + a)
    logger.info("b = " + b)

    assertSame(a.domain, 15)
    assertSame(b.domain, 5)
    
    logger.info("Finished test complex4")
  }

  def testComplex5() {
    logger.info("begin test complex5")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")
    
    c * a := 30
    a + b := 20
    a - b := 10

    logger.info("Propogating Constraints complex5")
    p.propogateConstraints
    state = p.findSolution
    
    logger.info("a = " + a)
    logger.info("b = " + b)
    logger.info("c = " + c)

    assertSame(a.domain, 15)
    assertSame(b.domain, 5)
    assertSame(c.domain, 2)
    
    logger.info("Finished test complex5")
  }

  def testComplex6() {
    logger.info("begin test complex6")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    
    a * b := 16
    a == b
    
    logger.info("Propogating Constraints complex6")
    p.propogateConstraints
    logger.info("a = " + a)
    logger.info("b = " + b)
    assertSame(a.domain, domain(1 upto 16))
    assertSame(b.domain, domain(1 upto 16))

    logger.info("Finding Solution complex6")
    state = p.findSolution    
    logger.info("a = " + a)
    logger.info("b = " + b)
    
    assertSame(a.domain, 4)
    assertSame(b.domain, 4)
    
    logger.info("Finished test complex6")
  }

  def testComplex7() {
    logger.info("begin test complex7")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    val c = p.newVar("c")
    
    a * b == c
    c := 24
    a / b := 6

    logger.info("Propogating Constraints complex7")
    p.propogateConstraints
    logger.info("a = " + a)
    logger.info("b = " + b)
    logger.info("c = " + c)
    assertSame(a.domain, domain(6 upto 24))
    assertSame(b.domain, domain(1 upto 4))
    assertSame(c.domain, 24)
    
    logger.info("Finding Solution complex7")
    state = p.findSolution
    logger.info("a = " + a)
    logger.info("b = " + b)
    logger.info("c = " + c)

    assertSame(a.domain, 12)
    assertSame(b.domain, 2)
    assertSame(c.domain, 24)

    logger.info("Finished test complex7")
  }

  def testComplex8() {
    logger.info("begin test complex8")
    val p = new Problem
    implicit val state = p.state
    val a = p.newVar("a", domain(0 upto 6, 8 upto 100))
    val b = p.newVar("b", domain(0 upto 2, 4 upto 20))
    
    a * b := 21

    logger.info("Propogating Constraints complex8")
    p.propogateConstraints		     
    logger.info("a = " + a)
    logger.info("b = " + b)
    assertSame(a.domain, domain(1 upto 6, 8 upto 21))
    assertSame(b.domain, domain(1 upto 2, 4 upto 20))

    logger.info("Finished test complex8")
  }

  def testComplex9() {
    logger.info("begin test complex9")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    
    a * b := 25
    a /= b
    a /= 1

    logger.info("Propogating Constraints complex9")
    p.propogateConstraints
    logger.info("a = " + a)
    logger.info("b = " + b)

    logger.info("Finding Solution complex9")
    state = p.findSolution
    logger.info("a = " + a)
    logger.info("b = " + b)

    assertSame(a.domain, 25)
    assertSame(b.domain, 1)
  }

/*  def testComplex10() {
    logger.info("begin test complex10")
    val p = new Problem
    implicit var state = p.state
    val nv = (s:String) => p.newVar(s,domain(0 upto 9))
    val (xxxxx, xxxx, xxx, xx) = (p.newVar("",domain(10000 upto 10000)),
				  p.newVar("",domain(1000 upto 1000)),
				  p.newVar("",domain(100 upto 100)),
				  p.newVar("",domain(10 upto 10)))
    val (s,e,n,d,m,o,r,y) = (nv("s"),nv("e"),
			     nv("n"),nv("d"),
			     nv("m"),nv("o"),
			     nv("r"),nv("y"))
    s /= 0
    m /= 0
    val v1 = xxxx*s + xxx*e + xx*n + d
    val v2 = xxxx*m + xxx*o + xx*r + e
    val v3 = xxxxx*m + xxxx*o + xxx*n + xx*e + y
    v1 + v2 == v3
    
    p.addConstraint(new DifferenceConstraint(s,e,n,d,m,o,r,y))

    p.propogateConstraints
    state = p.findSolution
    logger.info(Array(s,e,n,d,m,o,r,y).mkString(" "))
  }
*/      
    
}
