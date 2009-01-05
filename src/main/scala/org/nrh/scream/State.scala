package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

object State {
  def newState = new State(new HashMap[Var,VarState]())
}

class State(val varStates: HashMap[Var,VarState])
extends Logging {

  implicit val state = this

  def set(v:Var,s:VarState){
    varStates.update(v,s)
  }

  def stateOf(v:Var):VarState = varStates(v)

  def mimicWith(v:Var, vst:VarState):State = {
    val ns = new State(varStates.clone.asInstanceOf[HashMap[Var,VarState]])
    ns.set(v,vst)
    return ns
  }

  def vars = varStates.keys
  def userVars = vars.filter(_.isFromUser)
  def nextUnAssigned:Option[Var] = {
    if(userVars.forall(_.isAssigned)) {
      return None
    }
    else {
      val unassigned = userVars.filter(!_.isAssigned)
      val smallest = unassigned.reduceLeft((x,y) => {
	if(x.domain.length < y.domain.length) x else y
      })
      return Some(smallest)
    }
  }

  def allSatisfied:Boolean = vars.forall(v => v.isAssigned && !(v eq EmptyDomain))
  def unsatisfied:List[Var] = vars.filter(v => !v.isAssigned || (v eq EmptyDomain))

}
