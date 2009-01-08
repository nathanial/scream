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

    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)

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

    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)

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
    
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    logger.info("c = " + c.domain)

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
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)

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
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    logger.info("c = " + c.domain)

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
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    assertSame(a.domain, domain(1 upto 16))
    assertSame(b.domain, domain(1 upto 16))

    logger.info("Finding Solution complex6")
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    
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
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    logger.info("c = " + c.domain)
    assertSame(a.domain, domain(6 upto 24))
    assertSame(b.domain, domain(1 upto 4))
    assertSame(c.domain, 24)
    
    logger.info("Finding Solution complex7")
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    logger.info("c = " + c.domain)

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
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
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
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)

    logger.info("Finding Solution complex9")
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }    
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)

    assertSame(a.domain, 25)
    assertSame(b.domain, 1)
  }

  def testDifference1() {
    logger.info("begin test difference1")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a",singleton(1))
    val b = p.newVar("b",domain(0 upto 100))

    p.allDiff(a,b)
    p.propogateConstraints
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    assertSame(a.domain, 1)
    assertSame(b.domain, domain(0 upto 0, 
				2 upto 100))
  }
  
  def testDifference2() {
    logger.info("begin test difference2")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")

    a /= 1
    a * b := 25

    p.allDiff(a,b)
    p.propogateConstraints
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    assertSame(a.domain, domain(2 upto 25))
    assertSame(b.domain, domain(1 upto 12))
  }  

  def testSelfReference() {
    logger.info("begin test self-reference")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a", domain(0 upto 9))
    val b = p.newVar("b", domain(0 upto 9))

    a + b == a

    p.propogateConstraints
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    
    logger.info("Find Solution self-reference")
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    
    logger.info("a = " + a.domain)
    logger.info("b = " + b.domain)
    logger.info("Finished Testing self-reference")
  }

  def testSendMoreMoney() {
    logger.info("begin test SendMoreMoney")
    val p = new Problem
    implicit var state = p.state
    val nv = (s:String) => p.newVar(s,domain(0 upto 9))
    val (xxxxx, xxxx, xxx, xx) = (p.newVar("10000",singleton(10000)),
				  p.newVar("1000",singleton(1000)),
				  p.newVar("100",singleton(100)),
				  p.newVar("10",singleton(10)))
    val (s,e,n,d,m,o,r,y) = (nv("s"),nv("e"),
			     nv("n"),nv("d"),
			     nv("m"),nv("o"),
			     nv("r"),nv("y"))
    s /= 0
    m /= 0
    val v1 = (xxxx*s) + (xxx*e) + (xx*n) + d
    val v2 = (xxxx*m) + (xxx*o) +(xx*r) + e
    val v3 = (xxxxx*m) + (xxxx*o) + (xxx*n) + (xx*e) + y
    v1 + v2 == v3
    
    p.allDiff(s,e,n,d,m,o,r,y)
    logger.info("complex10 propogating constraints")
    p.propogateConstraints
    Array(s,e,n,d,m,o,r,y).foreach(x => logger.info(x.name + " = " + x.domain))
    logger.info("complex10 finding solution")
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    Array(s,e,n,d,m,o,r,y).foreach(x => logger.info(x.name + " = " + x.domain))
    assertSame(s.domain,9)
    assertSame(e.domain,5)
    assertSame(n.domain,6)
    assertSame(d.domain,7)
    assertSame(m.domain,1)
    assertSame(o.domain,0)
    assertSame(r.domain,8)
    assertSame(y.domain,2)
  }

  def testSudoku1() {
    logger.info("begin test Sudoku1")
    val p = new Problem
    implicit var state = p.state
    val toVar = new VarTransformer(p)
    
    val question = new Matrix[Int](
      0 :: 0 :: 0 :: 0 :: 8 :: 0 :: 0 :: 0 :: 0 ::
      0 :: 0 :: 0 :: 1 :: 0 :: 6 :: 5 :: 0 :: 7 ::
      4 :: 0 :: 2 :: 7 :: 0 :: 0 :: 0 :: 0 :: 0 ::
      0 :: 8 :: 0 :: 3 :: 0 :: 0 :: 1 :: 0 :: 0 ::
      0 :: 0 :: 3 :: 0 :: 0 :: 0 :: 8 :: 0 :: 0 ::
      0 :: 0 :: 5 :: 0 :: 0 :: 9 :: 0 :: 7 :: 0 ::
      0 :: 5 :: 0 :: 0 :: 0 :: 8 :: 0 :: 0 :: 6 ::
      3 :: 0 :: 1 :: 2 :: 0 :: 4 :: 0 :: 0 :: 0 ::
      0 :: 0 :: 6 :: 0 :: 1 :: 0 :: 0 :: 0 :: 0 :: Nil)

    val answer = new Matrix[Int](
      5 :: 6 :: 7 :: 4 :: 8 :: 3 :: 2 :: 9 :: 1 ::
      9 :: 3 :: 8 :: 1 :: 2 :: 6 :: 5 :: 4 :: 7 ::
      4 :: 1 :: 2 :: 7 :: 9 :: 5 :: 3 :: 6 :: 8 ::
      6 :: 8 :: 9 :: 3 :: 7 :: 2 :: 1 :: 5 :: 4 ::
      7 :: 4 :: 3 :: 6 :: 5 :: 1 :: 8 :: 2 :: 9 ::
      1 :: 2 :: 5 :: 8 :: 4 :: 9 :: 6 :: 7 :: 3 ::
      2 :: 5 :: 4 :: 9 :: 3 :: 8 :: 7 :: 1 :: 6 ::
      3 :: 7 :: 1 :: 2 :: 6 :: 4 :: 9 :: 8 :: 5 ::
      8 :: 9 :: 6 :: 5 :: 1 :: 7 :: 4 :: 3 :: 2 :: Nil)
    
    val puzzle = new Matrix[Var](question.map(toVar))

    puzzle.squares.foreach(s => p.allDiff(s:_*))
    puzzle.rows.foreach(r => p.allDiff(r:_*))
    puzzle.columns.foreach(c => p.allDiff(c:_*))

    logger.info("Propogating Sudoku1 Constraints")
    p.propogateConstraints
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    val toInt = new BigIntTransformer(state)
    val solution = new Matrix(puzzle.map(toInt))
    logger.info(solution.toString)
    assert(solution sameAs answer)
  }

  def testSudoku2() {
    logger.info("begin test Sudoku2")
    val p = new Problem
    implicit var state = p.state
    val toVar = new VarTransformer(p)
    
    val question = new Matrix[Int](
      0 :: 0 :: 6 :: 1 :: 7 :: 0 :: 2 :: 9 :: 0 ::
      0 :: 0 :: 1 :: 3 :: 8 :: 0 :: 4 :: 6 :: 0 ::
      3 :: 9 :: 0 :: 0 :: 6 :: 0 :: 0 :: 0 :: 0 ::
      0 :: 0 :: 0 :: 6 :: 1 :: 0 :: 0 :: 2 :: 0 ::
      0 :: 1 :: 8 :: 0 :: 0 :: 0 :: 6 :: 7 :: 0 ::
      0 :: 5 :: 0 :: 0 :: 2 :: 7 :: 0 :: 0 :: 0 ::
      0 :: 0 :: 0 :: 0 :: 4 :: 0 :: 0 :: 8 :: 2 ::
      0 :: 7 :: 2 :: 0 :: 9 :: 8 :: 3 :: 0 :: 0 ::
      0 :: 4 :: 5 :: 0 :: 3 :: 6 :: 7 :: 0 :: 0 :: Nil)

    val answer = new Matrix[Int](
      4 :: 8 :: 6 :: 1 :: 7 :: 5 :: 2 :: 9 :: 3 ::
      5 :: 2 :: 1 :: 3 :: 8 :: 9 :: 4 :: 6 :: 7 ::
      3 :: 9 :: 7 :: 4 :: 6 :: 2 :: 1 :: 5 :: 8 ::
      7 :: 3 :: 9 :: 6 :: 1 :: 4 :: 8 :: 2 :: 5 ::
      2 :: 1 :: 8 :: 9 :: 5 :: 3 :: 6 :: 7 :: 4 ::
      6 :: 5 :: 4 :: 8 :: 2 :: 7 :: 9 :: 3 :: 1 ::
      9 :: 6 :: 3 :: 7 :: 4 :: 1 :: 5 :: 8 :: 2 ::
      1 :: 7 :: 2 :: 5 :: 9 :: 8 :: 3 :: 4 :: 6 ::
      8 :: 4 :: 5 :: 2 :: 3 :: 6 :: 7 :: 1 :: 9 :: Nil)

    val puzzle = new Matrix[Var](question.map(toVar))
    
    puzzle.squares.foreach(s => p.allDiff(s:_*))
    puzzle.rows.foreach(r => p.allDiff(r:_*))
    puzzle.columns.foreach(c => p.allDiff(c:_*))

    logger.info("Propogating Sudoku2 Constraints")
    p.propogateConstraints
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    val toInt = new BigIntTransformer(state)
    val solution = new Matrix(puzzle.map(toInt))
    logger.info(solution.toString)
    assert(solution sameAs answer)
  }
   

  def testSudoku3() {
    logger.info("begin test Sudoku3")
    val p = new Problem
    implicit var state = p.state
    val toVar = new VarTransformer(p)
    
    val question = new Matrix[Int]( 
      0 :: 4 :: 0 :: 0 :: 5 :: 3 :: 0 :: 0 :: 0 :: 
      0 :: 0 :: 0 :: 2 :: 0 :: 0 :: 0 :: 3 :: 8 ::
      0 :: 0 :: 0 :: 7 :: 0 :: 0 :: 6 :: 5 :: 0 ::
      0 :: 0 :: 6 :: 8 :: 0 :: 0 :: 0 :: 0 :: 1 ::
      0 :: 8 :: 0 :: 0 :: 0 :: 0 :: 0 :: 7 :: 0 :: 
      2 :: 0 :: 0 :: 0 :: 0 :: 7 :: 4 :: 0 :: 0 ::
      0 :: 3 :: 7 :: 0 :: 0 :: 9 :: 0 :: 0 :: 0 ::
      9 :: 5 :: 0 :: 0 :: 0 :: 2 :: 0 :: 0 :: 0 ::
      0 :: 0 :: 0 :: 4 :: 3 :: 0 :: 0 :: 1 :: 0 :: Nil)

    val answer = new Matrix[Int](
      6 :: 4 :: 8 :: 9 :: 5 :: 3 :: 1 :: 2 :: 7 ::
      7 :: 1 :: 5 :: 2 :: 4 :: 6 :: 9 :: 3 :: 8 ::
      3 :: 2 :: 9 :: 7 :: 1 :: 8 :: 6 :: 5 :: 4 ::
      5 :: 7 :: 6 :: 8 :: 2 :: 4 :: 3 :: 9 :: 1 ::
      4 :: 8 :: 3 :: 5 :: 9 :: 1 :: 2 :: 7 :: 6 ::
      2 :: 9 :: 1 :: 3 :: 6 :: 7 :: 4 :: 8 :: 5 ::
      1 :: 3 :: 7 :: 6 :: 8 :: 9 :: 5 :: 4 :: 2 :: 
      9 :: 5 :: 4 :: 1 :: 7 :: 2 :: 8 :: 6 :: 3 ::
      8 :: 6 :: 2 :: 4 :: 3 :: 5 :: 7 :: 1 :: 9 :: Nil)

    val puzzle = new Matrix[Var](question.map(toVar))
    
    puzzle.squares.foreach(s => p.allDiff(s:_*))
    puzzle.rows.foreach(r => p.allDiff(r:_*))
    puzzle.columns.foreach(c => p.allDiff(c:_*))
    
    logger.info("Propogating Sudoku3 Constraints")
    p.propogateConstraints
    p.findSolution match {
      case None => assert(false)
      case Some(s) => state = s
    }
    val toInt = new BigIntTransformer(state)
    val solution = new Matrix(puzzle.map(toInt))
    logger.info(solution.toString)
    assert(solution sameAs answer)
  }
}

class BigIntTransformer(state:State) extends Function[Var,BigInt] {
  def apply(v:Var):BigInt = {
    v.domain(state).toBigInt
  }
}
  

class VarTransformer(p:Problem) extends Function[Int,Var] {
  var (x,y) = (0,0)

  def apply(i:Int):Var = {
    val (_x,_y) = (x,y)
    if(x == 8){
      x = 0
      y += 1
    }
    else {
      x += 1
    }
    if(i > 0){
      val v = p.newVar("("+_x+","+_y+")",singleton(i))
      return v
    }
    else {
      val v = p.newVar("("+_x+","+_y+")",domain(1 upto 9))
      return v
    }    
  }
}

