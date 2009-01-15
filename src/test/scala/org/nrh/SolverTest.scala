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

/*  def testSolver2() = {
    logger.info("begin test solver2")
    val p = Problem.genSudoku
    val toVar = new VarTransformer(p)
    val toInt = new BigIntTransformer
    
    val puzzle = new Matrix[Var]((for(x <- 0 to 81) yield 0).map(toVar).toList)
    puzzle.squares.foreach(s => p.allDiff(s:_*))
    puzzle.rows.foreach(r => p.allDiff(r:_*))
    puzzle.columns.foreach(c => p.allDiff(c:_*))
    
    p.firstSolution
    logger.info(new Matrix(puzzle.map(toInt)).toString)
  }
*/

  def testSolver3() = {
    logger.info("begin test solver3")
    val p = Problem.genSudoku
    val a = p.newVar("a")
    val b = p.newVar("b")
    a * b := 20
    p.propogateConstraints
    val solutions = p.allSolutions.take(5)
    for(s <- solutions){
      s.get.assignToVars
      logger.info("a = " + a.domain)
      logger.info("b = " + b.domain)
    }
  }

}
