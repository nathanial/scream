package org.nrh.scream
import org.nrh.scream.Debug._


abstract class Var(val name: String, val problem: Problem, var domain:Domain) {
  var changed = false
  def +(that:Var):Var
  def -(that:Var):Var
  def /(that:Var):Var
  def *(that:Var):Var
  def ==(that:Var):Var
  def :=(that:Var):Var
  def :=(that:Domain):Var
  def isAssigned:Boolean = domain.min == domain.max

  override def toString:String = this.name
}

class DomainVar(n: String, p: Problem,d:Domain) 
extends Var(n,p,d) {
  def +(that:Var):Var = {
    val (x,y,z) = (this,that,problem.newVar)
    problem.addConstraint(new Addition(x,y,z))
    return z
  }

  def -(that:Var):Var = {
    val (x,y,z) = (this,that,problem.newVar)
    problem.addConstraint(new Subtraction(x,y,z))
    return z
  }
   
  def /(that:Var):Var = {
    val (x,y,z) = (this,that,problem.newVar)
    problem.addConstraint(new Division(x,y,z))
    return z
  }

  def *(that:Var):Var = {
    val (x,y,z) = (this,that,problem.newVar)
    problem.addConstraint(new Multiplication(x,y,z))
    return z
  }

  def ==(that:Var):Var = {
    val (x,y) = (this,that)
    problem.addConstraint(new Equality(x,y))
    return this
  }

  def :=(that:Var):Var = this := that.domain
    
  def :=(that:Domain):Var = {
    debug("---------------")
    debug(this.name + " := " + that)
    val nd = this.domain intersect that
    if(this.domain != nd){
      debug("setting " + this.name + " to " + nd)
      this.domain = nd
      changed = true
    }
    debug("---------------")
    return this
  }
}
