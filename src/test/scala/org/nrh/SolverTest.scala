package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.DomainImplicits._
import org.nrh.scream.Interval._
import org.nrh.scream.IntervalImplicits._
import org.scalatest._

object SolverTest {
  def main(args: Array[String]){
    (new SolverTest).execute()
  }
}

class SolverTest extends Suite with Logging {
  def assertSame(a:Domain, b:Domain){
    assert(a == b)
  }

  def testSolver1() {
    logger.info("Begin test solver1")
    val p = new Problem
    implicit var state = p.state
    val a = p.newVar("a")
    val b = p.newVar("b")
    a * b := 20
    p.propogateConstraints
    val solutions = p.findSolutions.collect.filter(_ != None).map(_.get)
    assert(solutions.exists(s => {
      state = s
      a.domain == singleton(20) &&
      b.domain == singleton(1)
    }))
    assert(solutions.exists(s => {
      state = s
      a.domain == singleton(10) && 
      b.domain == singleton(2)
    }))
    assert(solutions.exists(s => {
      state = s
      a.domain == singleton(5) && 
      b.domain == singleton(4)
    }))
    assert(solutions.exists(s => {
      state = s 
      a.domain == singleton(4) && 
      b.domain == singleton(5)
    }))
    assert(solutions.exists(s => {
      state = s 
      a.domain == singleton(2) &&
      b.domain == singleton(10)
    }))
    assert(solutions.exists(s => {
      state = s 
      a.domain == singleton(1) &&
      b.domain == singleton(20)
    }))
  }

  def testSolver2() = {
    logger.info("begin test solver2")
    val p = new Problem
    implicit var state = p.state
    val toVar = new VarTransformer(p)
    
    val puzzle = new Matrix[Var]((for(x <- 0 to 81) yield 0).map(toVar).toList)
    puzzle.squares.foreach(s => p.allDiff(s:_*))
    puzzle.rows.foreach(r => p.allDiff(r:_*))
    puzzle.columns.foreach(c => p.allDiff(c:_*))
    
    val solutions = p.findSolutions
    for(s <- solutions.take(5)){      
      val toInt = new BigIntTransformer(s.get)
      logger.info(new Matrix(puzzle.map(toInt)).toString)
    }
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

