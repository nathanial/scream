package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

object State {
  def newState = new State(new HashMap[Var,VarState]())
}

class State(val varStates: HashMap[Var,VarState])
extends Ordered[State] with Logging {

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
  def allSatisfied = vars.forall(_.isSatisfied)
  def unsatisfied = vars.filter(!_.isSatisfied)
  def assigned = vars.filter(_.isAssigned).toList
  def unassigned = vars.filter(!_.isAssigned)
  def allAssigned = vars.forall(_.isAssigned)
  def consistent = !vars.exists(_.domain eq EmptyDomain)

  def compare(that:State) = {
    val len1 = this.assigned.length
    val len2 = that.assigned.length
    if(len1 > len2) 1
    else if(len1 == len2) 0
    else -1
  }

  override def toString:String = {
    userVars.map(v => {
      "("+v.name+" -> "+v.domain+")"
    }).mkString(" ")
  }
}
