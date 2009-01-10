package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._

abstract class Solver(val propogateConstraints:ConstraintPropogator,
		      val nextVariable:VariableSelector)
extends Iterator[Option[State]] {
  val trail = new Stack[Iterator[State]]
  var first_solution:Option[State] = None
  var first_time = true

  sealed def solve(state:State) = {
    restart
    first_solution = findSolution(state)
  }

  def next:Option[State] = {
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
}

abstract class ConstraintPropogator extends Function[State,Unit]
abstract class VariableSelector extends Function[State,Var]

class BacktrackingSolver(private val cp: ConstraintPropogator,
			 private val vs: VariableSelector)
extends Solver(cp,vs) with Logging
{
  def findSolution(state:State):Option[State] = {
    if(state.allAssigned){
      if(!state.allSatisfied) return None
      logger.debug("Solution = " + state)
      return Some(state)
    }
    else {
      logger.debug("State = "+state) 
      val next = nextVariable(state)
      val permutations = next.domain(state).map(
	x => state.mimicWith(next,next.mimicAssign(singleton(x))(state))
      ).elements

      var result:Option[State] = None
      while(result == None && permutations.hasNext){
	val nstate = permutations.next
	propogateConstraints(nstate)
	if(nstate.consistent){
	  result = findSolution(nstate)
	}
      }
      if(result != None) trail.push(permutations)
      return result
    }
  }    
}

object AC3 extends ConstraintPropogator with Logging {
  def apply(_state:State) {
    implicit val state = _state
    val queue = new Queue[Var]
    queue ++= state.vars
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

object MRV extends VariableSelector {   
  def apply(state:State):Var = {
    state.unassigned.reduceLeft(choose(_,_)(state))
  }
  private def choose(x:Var,y:Var)(implicit state:State):Var = {
    if(x.domain < y.domain) x else y
  }
}
