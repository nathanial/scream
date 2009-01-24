package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer,HashMap}
import scala.util.Random
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._
import org.nrh.scream.Solution._

case class BacktrackingSolverConfiguration(
  propogateConstraints:ConstraintPropogator,
  nextVariable:VariableSelector,
  max_depth:Int,
  elementExtractor: Domain => Iterator[BigInt])
					   

class BacktrackingSolver(configuration: BacktrackingSolverConfiguration)
extends Solver with Logging
{
  val propogateConstraints = configuration.propogateConstraints
  val nextVariable = configuration.nextVariable
  val max_depth = configuration.max_depth
  val elementExtractor = configuration.elementExtractor

  val trace = new Queue[(State,Assignment)]
  var depth = 0

  def propogate(changed:List[Var]) { propogateConstraints(changed) }

  def firstSolution(csp:CSP):Option[Solution] = {
    if(depth >= max_depth) return None

    if(csp.allAssigned){
      if(!csp.allSatisfied) return None
      logger.debug("Solution = " + csp)
      val s = solution(csp.vars)
      publish(s)
      return Some(s)
    }
    else {
      depth += 1
      logger.debug("CSP depth({}) = {}",depth,csp) 
      publish(State(csp.vars))
      val nv = nextVariable(csp)
      val assignments  = elementExtractor(nv.domain)
      val result = doAssignments(Assignment(nv,assignments), csp)
      depth -= 1
      return result
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
      bind(csp.vars) {
	nv assign singleton(assignment)
	propogateConstraints(nv :: Nil)
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
