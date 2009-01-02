package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

object State {
  def newState = new State(new ListBuffer[Constraint](), 
				  new HashMap[Var,VarState](),
				  new HashMap[Var,Boolean]())
}

class State(val constraints: ListBuffer[Constraint], 
		      val varStates: HashMap[Var,VarState],
		      val changed: HashMap[Var,Boolean])
extends Logging {

  def add(c:Constraint){
    constraints += c
  }

  def set(v:Var,s:VarState){
    varStates.update(v,s)
    changed.update(v,true)
  }

  def stateOf(v:Var):VarState = varStates(v)
  def hasChanged(v:Var):Boolean = changed(v)
  def setChanged(v:Var, b:boolean) { changed.update(v,b)}

  def mimicWith(v:Var, vst:VarState):State = {
    val ns = new State(constraints.clone.asInstanceOf[ListBuffer[Constraint]],
				 varStates.clone.asInstanceOf[HashMap[Var,VarState]],
				 changed.clone.asInstanceOf[HashMap[Var,Boolean]])
    ns.set(v,vst)
    return ns
  }

  def vars = varStates.keys

}
