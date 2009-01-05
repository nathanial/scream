package org.nrh.scream
import org.nrh.scream.Interval._
import org.nrh.scream.Domain._

abstract class Var {
  var name = "default"

  def domain(implicit state:State):Domain
  def hasChanged(implicit state:State):Boolean
  def setChanged(b:Boolean)(implicit state:State)
  def isFromUser(implicit state:State):Boolean

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
}

object Var {
  def newVar(state:State):Var = newVar(Domain.domain(interval(0,100000000)),state)

  def newVar(d:Domain,state:State):Var = {
    val v = new DomainVar
    val vst = new VarState(d)
    state.set(v,vst)
    return v
  }
  
  def newVar(d:Domain,state:State,fromUser:Boolean):Var = {
    val v = new DomainVar
    val vst = new VarState(d)
    state.set(v,vst)
    state.fromUser(v, fromUser)
    return v
  }
}

class DomainVar extends Var with Logging {
  import org.nrh.scream.Var._

  def domain(implicit state:State) = state.stateOf(this).domain
  def hasChanged(implicit state:State) = state.hasChanged(this)
  def setChanged(b:Boolean)(implicit state:State) = state.setChanged(this,b)
  def isFromUser(implicit state:State):Boolean = state.isFromUser(this)

  override def toString:String = this.name

  def +(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    state.add(new AdditionConstraint(x,y,z))
    return z
  }

  def -(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    state.add(new SubtractionConstraint(x,y,z))
    return z
  }
   
  def /(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    state.add(new DivisionConstraint(x,y,z))
    return z
  }

  def *(that:Var)(implicit state:State):Var = {
    val (x,y,z) = (this,that,newVar(state))
    state.add(new MultiplicationConstraint(x,y,z))
    return z
  }

  def ==(that:Var)(implicit state:State):Var = {
    val (x,y) = (this,that)
    state.add(new EqualityConstraint(x,y))
    return this
  }

  def /=(that:Var)(implicit state:State):Var = {
    val (x,y) = (this,that)
    state.add(new InEqualityConstraint(x,y))
    return this
  }

  def /=(that:Domain)(implicit state:State):Var = {
    val (x,y) = (this,newVar(that,state))
    state.add(new InEqualityConstraint(x,y))
    return this
  }

  def :=(that:Var)(implicit state:State):Var = this := that.domain(state)
    
  def :=(that:Domain)(implicit state:State):Var = {
    logger.debug("{} := {}",this,that)
    val nd = this.domain intersect that
    if(this.domain != nd){
      logger.debug("setting {} to {}", this.name, nd)
      state.set(this, new VarState(nd))
    }
    return this
  }

  def assign(that:Domain)(implicit state:State) {
    logger.debug("{} assign {}",this,that)
    state.set(this, new VarState(that))
  }
}

class VarState(val domain: Domain, val changed: Boolean,
	       val fromUser: Boolean, val constraints:List[Constraint]) 
{
  def isAssigned:Boolean = domain.isSingleton
  override def toString = "(VS "+domain+")"

  def mimicWith(domain:Domain):VarState = {
    new VarState(domain, this.changed, this.fromUser, this.constraints)
  }
  def mimicWith(changed:Boolean):VarState = {
    new VarState(this.domain, changed, this.fromUser, this.constraints)
  }
  def mimicWith(domain:Domain, changed:Boolean):VarState = {
    new VarState(domain, changed, this.fromUser, this.constraints)
  } 
}
