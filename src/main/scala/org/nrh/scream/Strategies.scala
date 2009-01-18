package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer,HashMap}
import scala.util.Random
import org.nrh.scream.Util._

abstract class ConstraintPropogator extends Function[CSP,Unit]
abstract class VariableSelector extends Function[CSP,Var]

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

//Most Restricted (unassigned) Variable
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
    val conflictedVars = csp.conflicted.filter(_.isFromUser).toList
    return chooseRandomly(conflictedVars)
  }
}

//Random Unassigned Variable
object RUV extends VariableSelector {
  def apply(csp:CSP):Var = {
    val v = chooseRandomly(csp.unassigned)
    println("RUV chose " + v.name)
    return v
  }
}
    
