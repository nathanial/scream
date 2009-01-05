package org.nrh.scream
import org.nrh.scream.Interval._
import org.nrh.scream.Domain._

abstract class Var {
  var name = "default"

  def domain(implicit state:State):Domain
  def isFromUser(implicit state:State):Boolean
  def constraints(implicit state:State):List[Constraint]
  def myState(implicit state:State):VarState

  def constrain(c:Constraint)(implicit state:State)
  
  def +(that:Var)(implicit state:State):Var
  def -(that:Var)(implicit state:State):Var
  def /(that:Var)(implicit state:State):Var
  def *(that:Var)(implicit state:State):Var
  def ==(that:Var)(implicit state:State):Var
  def :=(that:Var)(implicit state:State):Var
  def :=(that:Domain)(implicit state:State):Var
  def /=(that:Var)(implicit state:State):Var
  def /=(that:Domain)(implicit state:State):Var

  def assign(that:Domain)(implicit state:State)

  def isAssigned(implicit state:State):Boolean = domain.isSingleton
  def isSatisfied(implicit state:State):Boolean
}

object Var {
  def newVar(state:State):Var = newVar(state,Domain.domain(interval(0,100000000)), Nil)
  def newVar(state:State,d:Domain):Var = newVar(state,d,Nil)

  def newVar(state:State,d:Domain,constraints:List[Constraint]):Var = {
    newVar(state,d,false,constraints)
  }
  
  def newVar(state:State, d:Domain, fromUser:Boolean, constraints:List[Constraint]):Var = {
    val v = new DomainVar
    val vst = new VarState(d,fromUser,constraints)
    state.set(v,vst)
    v.name = "default" + count
    count += 1

    return v
  }

  var count = 0
}

class VarState(val domain: Domain, val fromUser: Boolean, val constraints:List[Constraint]) 
{
  def isAssigned:Boolean = domain.isSingleton
  override def toString = "(VS "+domain+")"

  def mimicWith(domain:Domain):VarState = {
    new VarState(domain, this.fromUser, this.constraints)
  }

  def mimicWith(constraint:Constraint):VarState = {
    new VarState(this.domain, this.fromUser, constraint :: this.constraints)
  }
}


class DomainVar extends Var with Logging {
  import org.nrh.scream.Var._

  def domain(implicit state:State) = myState.domain

  def constraints(implicit state:State) = myState.constraints

  def isFromUser(implicit state:State):Boolean = myState.fromUser

  def isSatisfied(implicit state:State):Boolean = myState.constraints.forall(_.isSatisfied)

  def constrain(constraint:Constraint)(implicit state:State){
    val vst = state.stateOf(this)
    val nvst = vst.mimicWith(constraint)
    state.set(this,nvst)
  }

  protected def constrainAll(c:Constraint,vars:Var*)(implicit state:State){
    for(v <- vars){
      v constrain c
    }
  }
  
  def myState(implicit state:State) = state.stateOf(this)

  override def toString:String = this.name

  def +(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    val add = new AdditionConstraint(x,y,z)
    constrainAll(add,x,y,z)
    return z
  }

  def -(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    val sub = new SubtractionConstraint(x,y,z)
    constrainAll(sub,x,y,z)
    return z
  }
   
  def /(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    val div = new DivisionConstraint(x,y,z)
    constrainAll(div,x,y,z)
    return z
  }

  def *(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    val mult = new MultiplicationConstraint(x,y,z)
    constrainAll(mult,x,y,z)
    return z
  }

  def ==(that:Var)(implicit state:State):Var = {
    val (x,y) = (this,that)
    val eq = new EqualityConstraint(x,y)
    constrainAll(eq,x,y)
    return this
  }

  def /=(that:Var)(implicit state:State):Var = {
    val (x,y) = (this,that)
    val neq = new InEqualityConstraint(x,y)
    constrainAll(neq,x,y)
    return this
  }

  def /=(that:Domain)(implicit state:State):Var = {
    val (x,y) = (this,newVar(state,that))
    return x./=(y)
  }

  def :=(that:Var)(implicit state:State):Var = this := that.domain(state)
    
  def :=(that:Domain)(implicit state:State):Var = {
    logger.debug("{} := {}",this,that)
    val nd = this.domain intersect that
    if(this.domain != nd){
      logger.debug("setting {} to {}", this.name, nd)
      state.set(this, myState.mimicWith(nd))
    }
    return this
  }

  def assign(that:Domain)(implicit state:State) {
    logger.debug("{} assign {}",this,that)
    state.set(this, myState.mimicWith(that))
  }
}
