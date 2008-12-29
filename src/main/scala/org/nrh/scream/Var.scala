package org.nrh.scream
import scala.util.logging.Logged


abstract class Var(val name: String, var domain:Domain) extends Logged {
  var changed = false
  def +(that:Var):Var
  def -(that:Var):Var
  def /(that:Var):Var
  def *(that:Var):Var
  def ==(that:Var):Var
  def :=(that:Var):Var
  def :=(that:Domain):Var

  def isSingleton:Boolean = domain.min == domain.max

  override def toString:String = this.name
}

class DomainVar(n: String, p: Problem,d:Domain) 
extends Var(n,d) {
  implicit def debugID:Symbol = 'DomainVar

  def +(that:Var):Var = {
    val (x,y,z) = (this,that,p.newVar)
    p.addConstraint(new Addition(x,y,z))
    return z
  }

  def -(that:Var):Var = {
    val (x,y,z) = (this,that,p.newVar)
    p.addConstraint(new Subtraction(x,y,z))
    return z
  }
   
  def /(that:Var):Var = {
    val (x,y,z) = (this,that,p.newVar)
    p.addConstraint(new Division(x,y,z))
    return z
  }

  def *(that:Var):Var = {
    val (x,y,z) = (this,that,p.newVar)
    p.addConstraint(new Multiplication(x,y,z))
    return z
  }

  def ==(that:Var):Var = {
    val (x,y) = (this,that)
    p.addConstraint(new Equality(x,y))
    return this
  }

  def :=(that:Var):Var = this := that.domain
    
  def :=(that:Domain):Var = {
    log("---------------")
    log(this.name + " := " + that)
    val nd = this.domain intersect that
    if(this.domain != nd){
      log("setting " + this.name + " to " + nd)
      this.domain = nd
      changed = true
    }
    log("---------------")
    return this
  }
}
