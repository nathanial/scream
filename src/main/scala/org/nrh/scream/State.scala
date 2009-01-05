package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

object State {
  def newState = new State(new ListBuffer[Constraint](), 
			   new HashMap[Var,VarState](),
			   new HashMap[Var,Boolean](),
			   new HashMap[Var,Boolean]())
}

class State(val varStates: HashMap[Var,VarState])
extends Logging {

  implicit val state = this

  def add(c:Constraint){
    constraintsList += c
  }

  def set(v:Var,s:VarState){
    varStates.update(v,s)
    changedMap.update(v,true)
  }

  def stateOf(v:Var):VarState = varStates(v)
  def hasChanged(v:Var):Boolean = changedMap(v)
  def setChanged(v:Var, b:boolean) { changedMap.update(v,b)}

  def mimicWith(v:Var, vst:VarState):State = {
    val ns = new State(constraintsList.clone.asInstanceOf[ListBuffer[Constraint]],
		       varStates.clone.asInstanceOf[HashMap[Var,VarState]],
		       changedMap.clone.asInstanceOf[HashMap[Var,Boolean]],
		       fromUser.clone.asInstanceOf[HashMap[Var,Boolean]])
    ns.set(v,vst)
    return ns
  }

  def vars = varStates.keys
  def unsatisfied = constraintsList.filter(!_.isSatisfied)
  def allSatisfied:Boolean = constraintsList.forall(_.isSatisfied)
  def changed = vars.exists(_.hasChanged)
  def setAllChanged(b:Boolean) { vars.foreach(_.setChanged(b)) }
  def unassigned = vars.filter(!_.isAssigned)
  def userVars = vars.filter(_.isFromUser)
  def constraints = constraintsList.toList
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
  def fromUser(v:Var,b:Boolean) {
    fromUser.update(v,b)
  }

  def isFromUser(v:Var):Boolean = {
    val result = fromUser.get(v)
    result match {
      case None => {
	fromUser.update(v,false)
	false
      }
      case Some(v) => v
    }	
  }
    
}
