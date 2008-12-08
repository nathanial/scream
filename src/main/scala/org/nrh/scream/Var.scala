package org.nrh.scream
import org.nrh.scream.DomainArithmetic._

trait Var {
  var changed = false

  def +(that: Var):Var

  def -(that: Var):Var

  def /(that: Var):Var

  def *(that: Var):Var

  def ==(that: Var):Var

  def ==(that: BigInt):Var    

  def domain:Domain

  def setDomain(that:Domain)

  def propagate

  def restrict(thatDomain: Domain) {
    val newDomain = this.domain intersection thatDomain
    if(this.domain != newDomain){
      changed = true
      this.setDomain(newDomain)
      propagate
    }
  }
}


class DomainVar(var dmn: Domain) extends Var {
  def +(that: Var):Var = Operations.add(this,that)
  
  def -(that: Var):Var = Operations.subtract(this,that)

  def /(that: Var):Var = Operations.divide(this,that)

  def *(that: Var):Var = Operations.multiply(this,that)

  def ==(that: Var):Var = Operations.equals(this,that)

  def ==(that: BigInt):Var = 
    Operations.equals(this, new DomainVar(Domain.domain(that)))

  def domain:Domain = this.dmn

  def setDomain(that:Domain) {
    this.dmn = that
  }

  def propagate {
    "do nothing"
  }

  def hasChanged:Boolean = {
    val ret = changed
    changed = !changed
    return ret
  }

}


class HiddenVariable(root: OperationVar) extends Constraint {
  val vars = extractVars(root)
  val constraint = extractConstraint(root)

  def satisfy:Boolean = {
    var varChanged = true
    try {
      while(varChanged) {
	constraint.satisfy
	varChanged = vars.exists(_.hasChanged)
      }
      return true
    } catch {
      case (e:ConstraintException) => return false
    }
  }

  private def extractVars(node: Var):List[DomainVar] =  node match {
    case (v:DomainVar) => List[DomainVar](v)
    case OperationVar(x,y) => extractVars(x) ++ extractVars(y)
  }

  private def extractConstraint(node: Var):UnaryConstraint = null
}  


abstract class OperationVar(val x: Var, val y: Var) extends Var {

  def +(that: Var):Var = Operations.add(this,that)
  
  def -(that: Var):Var = Operations.subtract(this,that)
  def /(that: Var):Var = Operations.divide(this,that)

  def *(that: Var):Var = Operations.multiply(this,that)

  def ==(that: Var):Var = Operations.equals(this,that)

  def ==(that: BigInt):Var = 
    Operations.equals(this, new DomainVar(Domain.domain(that)))

}

object OperationVar {
  def unapply(v: OperationVar):Option[(Var,Var)] = {
    if(v == null)
      None
    else
      Some((v.x,v.y))
  }
}

class AdditionVar(x: Var, y: Var) extends OperationVar(x,y) {
  var domain = x.domain + y.domain
  propagate

  def propagate { 
    x restrict { this.domain + y.domain }
    y restrict { this.domain + x.domain }
  }
  def setDomain(that:Domain) { this.domain = that }
}

class SubtractionVar(x: Var, y: Var) extends OperationVar(x,y) {
  var domain = x.domain - y.domain
  propagate

  def propagate {
    x restrict { this.domain + y.domain }
    y restrict { this.domain + x.domain }
  }
  def setDomain(that:Domain) { this.domain = that }
}

class DivisionVar(x: Var, y: Var) extends OperationVar(x,y) {
  var domain = x.domain / y.domain
  propagate

  def propagate {
    x restrict { this.domain * y.domain }
    y restrict { this.domain * x.domain }
  }
  def setDomain(that:Domain) { this.domain = that }
}

class MultiplicationVar(x: Var, y: Var) extends OperationVar(x,y) {
  var domain = x.domain * y.domain
  propagate

  def propagate {
    x restrict { this.domain / y.domain }
    y restrict { this.domain / x.domain }
  }
  def setDomain(that:Domain) { this.domain = that }
}

class EqualityVar(x: Var, y: Var) extends OperationVar(x,y) {
  var domain = x.domain intersection y.domain
  propagate

  def propagate {
    x restrict domain
    y restrict domain
  }

  def setDomain(that:Domain) { this.domain = that }
}

object Operations {
  def add(x: Var, y: Var):Var = new AdditionVar(x,y)

  def subtract(x: Var, y: Var):Var = new SubtractionVar(x,y)

  def divide(x: Var, y: Var):Var = new DivisionVar(x,y)

  def multiply(x: Var, y: Var):Var = new MultiplicationVar(x,y)

  def equals(x: Var, y: Var):Var = new EqualityVar(x,y)    
}

