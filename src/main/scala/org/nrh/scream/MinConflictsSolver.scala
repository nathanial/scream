package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer,HashMap}
import scala.util.Random
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._
import org.nrh.scream.Solution._

//This class is a little gross.... definitely need to revisit it.
//and test it :D 

class MinConflictsSolver(private val propogateConstraints: ConstraintPropogator,
			 private val max_steps: Int)
extends Solver with Logging {

  def propogate(changed:List[Var]) { propogateConstraints(changed) }

  def firstSolution(csp:CSP):Option[Solution] = {
    bind(csp.vars) {
      val initial = randomInitialAssignment(csp)
      initial.assignToVars
      
      var last:Var = null
      var result:Option[Solution] = None
      for(i <- 1 to max_steps; if(result == None)){
	if(csp.isSolved){
	  logger.debug("Solution = " + csp)
	  result = Some(solution(csp.vars))
	} else {
	  logger.debug("CSP = " + csp)
	  var v:Var = null
	  while((v eq null) || (v eq last)){
	    v = RCV(csp)
	  }
	  last = v
	  val d = minConflicts(v)
	  v assign singleton(d)
	}
      }
      result
    }
  }

  def allSolutions(csp:CSP):Iterator[Option[Solution]] = {
    new SolutionIterator(csp)
  }

  private def minConflicts(v:Var):BigInt = {
    val unsat:Iterable[Int] = v.previousDomain.map(
      x => {
	v assign singleton(x)
	v.unsatisfiedConstraints.length
      })
    if(unsat.forall(x => unsat.forall(y => x == y))){
      chooseRandomly(v.previousDomain)
    }
    else {
      v.previousDomain.reduceLeft(
	(x,y) => {
	  v assign singleton(x)
	  val c1 = v.unsatisfiedConstraints.length

	  v assign singleton(y)
	  val c2 = v.unsatisfiedConstraints.length

	  if(c1 < c2) x else y
	}
      )
    }
  }

  private def randomInitialAssignment(csp:CSP):State = {
    val state = bind(csp.vars) {
      csp.vars.foreach {
	v => {
	  val choice = v.domain.randomizedElements.next
	  logger.debug("choice = " + choice)
	  v assign singleton(choice)
	}
      }
      State(csp.vars)
    }
    return state
  }

  private class SolutionIterator(csp:CSP) extends Iterator[Option[Solution]] {
    def hasNext:Boolean = true
    def next = firstSolution(csp)
  }
}
