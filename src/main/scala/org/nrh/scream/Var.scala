package org.nrh.scream

trait Var {
  def +(that: Var):Var

  def -(that: Var):Var

  def /(that: Var):Var

  def *(that: Var):Var

  def ==(that: Var):Var

  def ==(that: BigInt):Var    

}


class DomainVar(var domain: Domain) extends Var {
  private var changed = false

  def +(that: Var):Var = Operations.add(this,that)
  
  def -(that: Var):Var = Operations.subtract(this,that)

  def /(that: Var):Var = Operations.divide(this,that)

  def *(that: Var):Var = Operations.multiply(this,that)

  def ==(that: Var):Var = Operations.equals(this,that)

  def ==(that: BigInt):Var = 
    Operations.equals(this,
		      new DomainVar(
			new Domain(
			  new Range(that, that))))

  def restrict(thatDomain: Domain) {
    val newDomain = this.domain - thatDomain
    if(this.domain != newDomain){
      changed = true
      this.domain = newDomain
    }
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


class OperationVar(val x: Var, val y: Var) extends Var {

  def +(that: Var):Var = Operations.add(this,that)
  
  def -(that: Var):Var = Operations.subtract(this,that)
  def /(that: Var):Var = Operations.divide(this,that)

  def *(that: Var):Var = Operations.multiply(this,that)

  def ==(that: Var):Var = Operations.equals(this,that)

  def ==(that: BigInt):Var = 
    Operations.equals(this,
		      new DomainVar(
			new Domain(
			  new Range(that, that))))

}

object OperationVar {
  def apply(x: Var, y: Var):OperationVar = new OperationVar(x,y)
  def unapply(v: OperationVar):Option[(Var,Var)] = {
    if(v == null)
      None
    else
      Some((v.x,v.y))
  }
}

class AdditionVar(x: Var, y: Var) extends OperationVar(x,y)
class SubtractionVar(x: Var, y: Var) extends OperationVar(x,y)
class DivisionVar(x: Var, y: Var) extends OperationVar(x,y)
class MultiplicationVar(x: Var, y: Var) extends OperationVar(x,y)
class EqualityVar(x: Var, y: Var) extends OperationVar(x,y)

object Operations {
  def add(x: Var, y: Var):Var = new AdditionVar(x,y)

  def subtract(x: Var, y: Var):Var = new SubtractionVar(x,y)

  def divide(x: Var, y: Var):Var = new DivisionVar(x,y)

  def multiply(x: Var, y: Var):Var = new MultiplicationVar(x,y)

  def equals(x: Var, y: Var):Var = new EqualityVar(x,y)    
}

