package org.nrh.scream
import scala.collection.mutable.{Queue}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._

abstract class Solver(val constraintPropogator:ConstraintPropogator, 
		      val variableSelector:VariableSelector) 
extends Function[State,Option[State]]

abstract class ConstraintPropogator extends Function[State,Unit]
abstract class VariableSelector extends Function[State,Var]

class BacktrackingSolver(private val cp: ConstraintPropogator,
			 private val vs: VariableSelector)
extends Solver(cp,vs) with Logging
{
  def apply(root:State):Option[State] = {
    constraintPropogator(root)
    logger.debug("Root = " + root)
    findSolution(root,1) 
  }

  def findSolution(state:State, depth:Int):Option[State] = {
    if(state.allAssigned){
      if(!state.allSatisfied) return None
      logger.debug("Solution = " + state)
      return Some(state)
    }
    else {
      logger.debug("State at depth "+depth+" = "+state) 
      val nextVar = variableSelector(state)
      val permutations = nextVar.domain(state).map(
	x => state.mimicWith(nextVar,nextVar.mimicAssign(singleton(x))(state))
      ).elements

      var result:Option[State] = None
      while(result == None && permutations.hasNext){
	val nstate = permutations.next
	constraintPropogator(nstate)
	if(nstate.consistent){
	  result = findSolution(nstate,depth + 1)
	}
      }
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
    if(x.domain < y.domain) x
    else y
  }
}
  
