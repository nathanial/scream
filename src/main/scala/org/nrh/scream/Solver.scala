package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer,HashMap}
import scala.util.Random
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._
import org.nrh.scream.Solution._
import org.nrh.scream.CSP._

abstract class Solver(val propogateConstraints:ConstraintPropogator,
		      val nextVariable:VariableSelector)
{ 
  def findSolution(csp:CSP):Option[Solution]
}
 /*extends Iterator[Option[State]]{
  val trail = new Stack[Iterator[State]]
  var first_solution:Option[State] = None
  var first_time = true

  sealed def solve(state:State) = {
    restart
    first_solution = findSolution(state)
  }

  def next:Option[Solution] = {
    if(first_time){
      first_time = false
      return first_solution
    } else {
      if(!trail.isEmpty){
	var result:Option[State] = None
	while(result == None && !trail.isEmpty){
	  val choices = trail.pop
	  while(result == None && choices.hasNext){
	    result = findSolution(choices.next)
	  }
	  if(result != None && choices.hasNext) trail.push(choices)
	}
	return result
      }
      else {
	return None
      }
    }
  }

  def hasNext:Boolean = first_time || !trail.isEmpty

  private def restart {
    first_time = true
    trail.clear
    first_solution = None
  }

  protected def findSolution(state:State):Option[State]
}*/

abstract class ConstraintPropogator extends Function[CSP,Unit]
abstract class VariableSelector extends Function[CSP,Var]

class Solution(assignments:Map[Var,Domain]) {
  def assignToVars {
    assignments.foreach(item => item._1 assign item._2)
  }
}

object Solution {
  def solution(vars:Seq[Var]):Solution = {
    new Solution(Map(vars.map(v => (v,v.domain)):_*))
  }
}

class BacktrackingSolver(private val cp: ConstraintPropogator,
			 private val vs: VariableSelector)
extends Solver(cp,vs) with Logging
{
  def findSolution(csp:CSP):Option[Solution] = {
    if(csp.allAssigned){
      if(!csp.allSatisfied) return None
      logger.debug("Solution = " + csp)
      return Some(solution(csp.vars))
    }
    else {
      logger.debug("CSP = "+csp) 
      val nv = nextVariable(csp)
      val assignments  = nv.domain.elements

      var result:Option[Solution] = None
      while(result == None && assignments.hasNext){
	val assignment = assignments.next
	bind(csp){
	  nv assign singleton(assignment)
	  propogateConstraints(csp)
	  if(csp.isConsistent){
	    result = findSolution(csp)
	  }
	}
      }
      return result
    }
  }    
}

object AC3 extends ConstraintPropogator with Logging {
  def apply(csp:CSP) {
    val queue = new Queue[Var]
    queue ++= csp.vars
    while(!queue.isEmpty){
      val v = queue.dequeue
      for(c <- v.constraints){
	if(!c.isSatisfied){
	  val changed = c.propogate
	  changed.foreach(x => queue.enqueue(x))
	}
      }
    }
  }
}

//Most Restricted Variable
object MRV extends VariableSelector {   
  def apply(csp:CSP):Var = {
    csp.unassigned.reduceLeft(choose(_,_))
  }
  private def choose(x:Var,y:Var):Var = {
    if(x.domain < y.domain) x else y
  }
}

//Random Conflicted Variable
object RCV extends VariableSelector {
  def apply(csp:CSP):Var = {
    val conflictedVars = csp.conflicted.toList
    val random = new Random
    val i = random.nextInt % conflictedVars.length
    return conflictedVars(i)
  }
}

/*
class MinConflictsSolver(private val cp: ConstraintPropogator,
			 val max_steps: Int)
extends Solver(cp, RCV) with Logging {
  def findSolution(_state:State):Option[State] = {
    var state = _state
    for(i <- 1 to max_steps){
      if(state.isSolution){
	println("Solution = " + state)
	return Some(state)
      } else {
	val v = nextVariable(state)
	println("selected " + v)
	val d = v.domain(state).reduceLeft((x,y) => minConflicts(x,y,v,state))
	println("picked value " + d)
	state = state.mimicWith(v,v.mimicAssign(singleton(d))(state))
	propogateConstraints(state)
	println("State = "+state) 
      }
    }
    return None
  }

  def minConflicts(x:BigInt, y:BigInt, v:Var, _state:State) = {
    implicit val state = _state
    val xs = state.mimicWith(v,v.mimicAssign(singleton(x)))
    val ys = state.mimicWith(v,v.mimicAssign(singleton(y)))
    if(xs.unsatisfiedConstraints.length < ys.unsatisfiedConstraints.length){
      x
    }
    else {
      y
    }
  }
}
*/
