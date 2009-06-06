package org.nrh
import org.nrh.scream._
import org.nrh.scream.Domain._
import org.nrh.scream.DomainImplicits._
import org.nrh.scream.Interval._
import org.nrh.scream.IntervalImplicits._
import org.nrh.scream.Util._
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
    val p = Problem.standard
    val a = p.newVar("a")
    val b = p.newVar("b")
    a * b := 20
    p.propogateConstraints

    val solutions = p.allSolutions.collect.filter(_ != None).map(_.get)
    def solutionExistsWhere(test: => Boolean):Boolean = {
      solutions.exists(s => {
	s.assignToVars
	test
      })
    }

    assert(solutionExistsWhere(
      a.domain == singleton(20) &&
      b.domain == singleton(1)))

    assert(solutionExistsWhere(
      a.domain == singleton(10) && 
      b.domain == singleton(2)))

    assert(solutionExistsWhere(
      a.domain == singleton(5) && 
      b.domain == singleton(4)))

    assert(solutionExistsWhere(
      a.domain == singleton(4) && 
      b.domain == singleton(5)))

    assert(solutionExistsWhere(
      a.domain == singleton(2) &&
      b.domain == singleton(10)))

    assert(solutionExistsWhere(
      a.domain == singleton(1) &&
      b.domain == singleton(20)))
  }

  def testSolver3() = {
    logger.info("begin test solver3")
    val p = new Problem(new MinConflictsSolver(AC3,500))
    val a = p.newVar("a")
    val b = p.newVar("b")
    a * b := 20
    p.propogateConstraints
    val solutions = p.allSolutions.take(5)
    for(s <- solutions){
      bind(p.csp.vars) {
	println("before a = " + a.domain)
	println("before b = " + b.domain)
	s.get.assignToVars
	logger.info("a = " + a.domain)
	logger.info("b = " + b.domain)
      }
      println("after a = " + a.domain)
      println("after b = " + b.domain)
    }

  }

/*  def testNQueens() = {
    logger.info("begin test nqueens")
    val p = new PRoblem(new MinConflictsSolver(AC3,500))
    val q1 = p.newVar("q1",domain(1 upto 8))
    val q2 = p.newVar("q2",domain(1 upto 8))
    val q3 = p.newVar("q3",domain(1 upto 8))
    val q4 = p.newVar("q4",domain(1 upto 8))
    val q5 = p.newVar("q5",domain(1 upto 8))
    val q6 = p.newVar("q6",domain(1 upto 8))
    val q7 = p.newVar("q7",domain(1 upto 8))
    val q8 = p.newVar("q8",domain(1 upto 8))
    val queens = Array(q1,q2,q3,q4,q5,q6,q7,q8).toList
   
    p.allDiff(queens)
    for(queen <- queens){      
      queen /= 
      
    
    
  }
*/
}
