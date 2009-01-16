package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer,HashMap}
import scala.util.Random
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._
import org.nrh.scream.Solution._

abstract class Solver(val propogateConstraints:ConstraintPropogator,
		      val nextVariable:VariableSelector)
{
  def firstSolution(csp:CSP):Option[Solution]
  def allSolutions(csp:CSP):Iterator[Option[Solution]]
}

class BacktrackingSolver(private val cp: ConstraintPropogator,
			 private val vs: VariableSelector)
extends Solver(cp,vs) with Logging
{
  val trace = new Queue[(State,Assignment)]

  def firstSolution(csp:CSP):Option[Solution] = {
    if(csp.allAssigned){
      if(!csp.allSatisfied) return None
      logger.debug("Solution = " + csp)
      return Some(solution(csp.vars))
    }
    else {
      logger.debug("CSP = "+csp) 
      val nv = nextVariable(csp)
      val assignments  = nv.domain.elements
      return doAssignments(Assignment(nv,assignments), csp)
    }
  }    

  def allSolutions(csp:CSP):Iterator[Option[Solution]] = {
    new SolutionIterator(csp)
  }

  private class SolutionIterator(csp:CSP) extends Iterator[Option[Solution]] {
    var firstTime = true

    def hasNext:Boolean = firstTime || !trace.isEmpty
    def next:Option[Solution] = {
      if(firstTime){
	firstTime = false
	return firstSolution(csp)
      }
      else {
	val (state,assignment) = trace.dequeue
	state.assignToVars
	return doAssignments(assignment, csp)
      }
    }
  }

  private def doAssignments(a:Assignment, csp:CSP):Option[Solution] = {
    val nv = a.variable
    val assignments = a.assignments
    var result:Option[Solution] = None
    
    while(result == None && assignments.hasNext){
      val assignment = assignments.next
      csp.bind {
	nv assign singleton(assignment)
	propogateConstraints(csp)
	if(csp.isConsistent){
	  result = firstSolution(csp)
	}
      }
    }
    if(assignments.hasNext){
      trace += ((State(csp.vars), Assignment(nv, assignments)))
    }
    return result
  }
       
}

class MinConflictsSolver(private val cp: ConstraintPropogator,
			 val max_steps: Int)
extends Solver(cp, RCV) with Logging {
  def firstSolution(csp:CSP):Option[Solution] = {
    csp.pushNewDomains
    val initial = randomInitialAssignment(csp)
    initial.assignToVars

    var last:Var = null
    for(i <- 1 to max_steps){
      if(csp.isSolved){
	return Some(solution(csp.vars))
      } else {
	var v:Var = null
	while((v eq null) || (v eq last)){
	  v = nextVariable(csp)
	}
	last = v
	val d = minConflicts(v)
	v assign singleton(d)
      }
    }
    return None
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
    val random = new Random
    val state = csp.bind {
      csp.vars.foreach {
	v => {
	  val ri:BigInt = Math.abs(random.nextInt)
	  val offset:BigInt = ri % v.domain.length
	  val choice = v.domain.min + offset
	  verify(choice >= v.domain.min, "choice less than min! " + 
		 " choice = "+ choice +
		 " offset = "+ offset +
		 " ri = " + ri)
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


class State(assignments:Map[Var,Domain]){
  def assignToVars {
    assignments.foreach(item => item._1 assign item._2)
  }
}  

class Solution(a:Map[Var,Domain]) extends State(a)

object State {
  def apply(vars:Seq[Var]):State = {
    new State(Map(vars.map(v => (v,v.domain)):_*))
  }
}

object Solution {
  def solution(vars:Seq[Var]):Solution = {
    new Solution(Map(vars.map(v => (v,v.domain)):_*))
  }
}

class Assignment(val variable:Var, val assignments:Iterator[BigInt])

object Assignment {
  def apply(v:Var, nums:Iterator[BigInt]):Assignment = {
    new Assignment(v,nums)
  }
}
  
