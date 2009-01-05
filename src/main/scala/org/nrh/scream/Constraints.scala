package org.nrh.scream

trait Constraint extends Logging {
  def isSatisfied(implicit state:State):Boolean
  def propogate(implicit state:State):List[Var]

  protected def setVar(x:Var, y:Domain)(implicit state: State){
    if(!x.isAssigned){
      logger.debug("setVar "+x.name+" to "+y)
      x := y
    }
  }

  protected def consistent(a:Var, b:Domain)(implicit state:State):Boolean = {
    val result = a.domain subset b
    logger.debug(a.name + " subset " + b + " = " + result)
    return result
  }

  protected def varsInfo(vars:Var*)(implicit state: State):String = {
    vars.map(_.name).mkString(" ") + "|" + vars.map(_.domain).mkString(" ")
  }
  
  protected def changedVars(vars:Var*)(implicit state:State):List[Var] = {
    vars.filter(_.hasChanged).toList
  }
}

class AdditionConstraint(x:Var,y:Var,z:Var) 
extends Constraint with Logging {
  def isSatisfied(implicit state:State) = {
    trace("Addition Constraint on " + varsInfo(x,y,z)) {
      consistent(z, x.domain + y.domain) &&
      consistent(x, z.domain - y.domain) && 
      consistent(y, z.domain - x.domain)
    }
  }
  def propogate(implicit state:State):List[Var] = { 
    logger.debug("Propogating Addition on " + varsInfo(x,y,z))
    setVar(z, x.domain + y.domain)
    setVar(x, z.domain - y.domain)
    setVar(y, z.domain - x.domain)
    logger.debug("Finished Propogating Addition")
    return changedVars(x,y,z)
  }
}

class SubtractionConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  def isSatisfied(implicit state:State) = {
    trace("Subtraction Constraint on " + varsInfo(x,y,z)){
      consistent(z, x.domain - y.domain) &&
      consistent(x, z.domain + y.domain) &&
      consistent(y, x.domain - z.domain)
    }
  }
  def propogate(implicit state:State):List[Var] = { 
    logger.debug("Propogating Subtraction on " + varsInfo(x,y,z))
    setVar(z, x.domain - y.domain)
    setVar(x, z.domain + y.domain)
    setVar(y, x.domain - z.domain)
    logger.debug("Finished Propogating Subtraction")
    return changedVars(x,y,z)
  }
}

class MultiplicationConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  def isSatisfied(implicit state:State) = {
    trace("Multiplication Constraint on " + varsInfo(x,y,z)){
      consistent(z, x.domain * y.domain) &&
      consistent(x, z.domain / y.domain) &&
      consistent(y, z.domain / x.domain)
    }
  }
  def propogate(implicit state:State):List[Var] = { 
    logger.debug("Propogating Multiplication " + varsInfo(x,y,z))
    setVar(z, x.domain * y.domain)
    setVar(x, z.domain / y.domain)
    setVar(y, z.domain / x.domain)
    logger.debug("Finished Propogating Multiplication")
    return changedVars(x,y,z)
  }
}

class DivisionConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  def isSatisfied(implicit state:State) = {
    trace("Division Constraint on " + varsInfo(x,y,z)){
      consistent(z, x.domain / y.domain) &&
      consistent(x, z.domain * y.domain) &&
      consistent(y, x.domain / z.domain)
    }
  }
  def propogate(implicit state:State):List[Var] = { 
    logger.debug("Propogating Division " + varsInfo(x,y,z))
    setVar(z, x.domain / y.domain)
    setVar(x, z.domain * y.domain)
    setVar(y, x.domain / z.domain)
    logger.debug("Finished Propogating Divsion")
    return changedVars(x,y,z)
  }
}

class EqualityConstraint(x:Var, y:Var)
extends Constraint with Logging {
  def isSatisfied(implicit state:State) = {
    trace("Equality Constraint on " + varsInfo(x,y)){
      consistent(x, y.domain) &&
      consistent(y, x.domain)
    }
  }
  def propogate(implicit state:State):List[Var] = { 
    logger.debug("Propogating Equality on " + varsInfo(x,y))
    val intersection = x.domain intersect y.domain
    logger.debug("setVar "+x.name+" to "+intersection)
    logger.debug("setVar "+y.name+" to "+intersection)
    setVar(x, intersection)
    setVar(y, intersection)
    logger.debug("Finished Propogating Equality")
    return changedVars(x,y)
  }
}

class InEqualityConstraint(x:Var, y:Var)
extends Constraint with Logging {
  def isSatisfied(implicit state:State) = {
    trace("InEquality Constraint on " + varsInfo(x,y)){
      val intersection = x.domain intersect y.domain
      intersection eq EmptyDomain
    }
  }
  def propogate(implicit state:State):List[Var] = {
    if(x.isAssigned){
      logger.debug("setVar "+y.name+" to "+(y.domain remove x.domain))
      setVar(y, y.domain remove x.domain)
    }
    else if(y.isAssigned){
      logger.debug("setVar "+x.name+" to "+(x.domain remove y.domain))
      setVar(x, x.domain remove y.domain)
    }
    return changedVars(x,y)
  }
}

class DifferenceConstraint(_vars:Var*)
extends Constraint with Logging {
  val vars = _vars.toList
  def isSatisfied(implicit state:State) = {
    trace("Difference Constraints on " + varsInfo(_vars:_*)){
      val others = (x:Var) => {
	vars.remove(_ eq x)
      }
      vars.forall(v => 
	others(v).forall(o =>
	  (v.domain intersect o.domain) eq EmptyDomain)) 
    }
  }
  def propogate(implicit state:State):List[Var] = {
    logger.debug("Difference Constraint Propogating " + varsInfo(_vars:_*))
    val (assigned,unassigned) = vars.partition(_.isAssigned)
    for(x <- assigned){
      logger.debug(x.name + " is assigned to " + x.domain)
      for(y <- unassigned){
	if(x.domain overlap y.domain){
	  logger.debug(y.name + " = " + y.domain)
	  logger.debug(y.name + " remove " + x.name + " = " + (y.domain remove x.domain))
	  y assign (y.domain remove x.domain)
	  logger.debug(y.name + " = " + y.domain)
	}
      }
    }
    return changedVars(_vars:_*)
  }
    
}
