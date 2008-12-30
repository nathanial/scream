package org.nrh.scream

trait Constraint {
  def isSatisfied:Boolean
  def consistent(a:Var, b:Domain):Boolean = {
    a.domain subset b
  }
  def propogator:Propogator
}

class AdditionConstraint(x:Var,y:Var,z:Var) 
extends Constraint with Logging {
  val propogator = new AdditionPropogator(x,y,z)
  def isSatisfied = {
    consistent(z, x.domain + y.domain) &&
    consistent(x, z.domain - y.domain) && 
    consistent(y, z.domain - x.domain)
  }
}

class SubtractionConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  val propogator = new SubtractionPropogator(x,y,z)
  def isSatisfied = {
    consistent(z, x.domain - y.domain) &&
    consistent(x, z.domain + y.domain) &&
    consistent(y, x.domain - z.domain)
  }
}

class MultiplicationConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  val propogator = new MultiplicationPropogator(x,y,z)
  def isSatisfied = {
    consistent(z, x.domain * y.domain) &&
    consistent(x, z.domain / y.domain) &&
    consistent(y, z.domain / x.domain)
  }
}

class DivisionConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  val propogator = new DivisionPropogator(x,y,z)
  def isSatisfied = {
    consistent(z, x.domain / y.domain) &&
    consistent(x, z.domain * y.domain) &&
    consistent(y, x.domain / z.domain)
  }
}

class EqualityConstraint(x:Var, y:Var)
extends Constraint with Logging {
  val propogator = new EqualityPropogator(x,y)
  def isSatisfied = {
    consistent(x, y.domain) &&
    consistent(y, x.domain)
  }
}

class InEqualityConstraint(x:Var, y:Var)
extends Constraint with Logging {
  val propogator = new NonPropogator
  def isSatisfied = {
    !consistent(x, y.domain) ||
    !consistent(y, x.domain)
  }
}

class DifferenceConstraint(_vars:Var*)
extends Constraint with Logging {
  val vars = _vars.toList
  val propogator = new NonPropogator
  def isSatisfied = {
    val others = (x:Var) => {
      vars.remove(_ == x)
    }
    vars.forall(v => !others(v).exists(o => consistent(v,o.domain)))
  }
}
