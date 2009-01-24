package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer,HashMap}
import scala.util.Random
import org.nrh.scream.Util._
import org.nrh.scream.Profiler._

abstract class ConstraintPropogator extends Function[List[Var],Unit]
abstract class VariableSelector extends Function[CSP,Var]

object AC3 extends ConstraintPropogator with Logging {
  def apply(initialChanged:List[Var]) {
    timed('AC3){
      val queue = new Queue[Var]
      queue ++= initialChanged
      var count = 0
      while(!queue.isEmpty){
	count += 1
	val v = queue.dequeue
	for(c <- v.constraints){
	  timed('AC3_INNER_LOOP){
	    if(!c.isSatisfied){
	      val changed = c.propogate
//	      if(changed.exists(!_.isConsistent)) return;
	      changed.foreach(x => if(!queue.contains(x)) queue.enqueue(x))
	    }
	  }
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
    
