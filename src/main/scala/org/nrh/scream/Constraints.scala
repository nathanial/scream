package org.nrh.scream
import scala.collection.mutable.ListBuffer
import org.nrh.scream.Util.changes
import org.nrh.scream.Domain.singleton

trait Constraint extends Logging {
  def isSatisfied:Boolean
  def propogate:List[Var]

  protected def setVar(x:Var, y:Domain):Boolean = {
    changes(x.domain) {
      if(!x.isAssigned){
	x := y
      }
    }
  }

  protected def consistent(a:Var, b:Domain):Boolean = {
    val result = a.domain subset b
    logger.debug(a.name + " subset " + b + " = " + result)
    return result
  }

  protected def varsInfo(vars:Var*):String = {
    vars.map(_.name).mkString(" ") + "|" + vars.map(_.domain).mkString(" ")
  }

  protected def varNames(vars:Var*):String = {
    vars.map(_.name).mkString(" ")
  }

  def containsVar(v:Var):Boolean

}

class AdditionConstraint(x:Var,y:Var,z:Var) 
extends Constraint with Logging {
  def isSatisfied = {
    trace("Addition Constraint on " + varsInfo(x,y,z)) {
      consistent(z, x.domain + y.domain) &&
      consistent(x, z.domain - y.domain) && 
      consistent(y, z.domain - x.domain)
    }
  }
  def propogate:List[Var] = { 
    logger.debug("Propogating Addition on " + varsInfo(x,y,z))
    val changedVars = new ListBuffer[Var]

    if(setVar(z, x.domain + y.domain)) changedVars += z
    if(setVar(x, z.domain - y.domain)) changedVars += x
    if(setVar(y, z.domain - x.domain)) changedVars += y

    logger.debug("Finished Propogating Addition")
    return changedVars.toList
  }
  def containsVar(v:Var):Boolean = (v eq x) || (v eq y) || (v eq z)
  override def toString = "Addition Constraint " + varNames(x,y,z)
}

class SubtractionConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  def isSatisfied = {
    trace("Subtraction Constraint on " + varsInfo(x,y,z)){
      consistent(z, x.domain - y.domain) &&
      consistent(x, z.domain + y.domain) &&
      consistent(y, x.domain - z.domain)
    }
  }
  def propogate:List[Var] = { 
    logger.debug("Propogating Subtraction on " + varsInfo(x,y,z))
    val changedVars = new ListBuffer[Var]

    if(setVar(z, x.domain - y.domain)) changedVars += z
    if(setVar(x, z.domain + y.domain)) changedVars += x
    if(setVar(y, x.domain - z.domain)) changedVars += y

    logger.debug("Finished Propogating Subtraction")
    return changedVars.toList
  }
  def containsVar(v:Var):Boolean = (v eq x) || (v eq y) || (v eq z)
  override def toString = "Subtraction Constraint " + varNames(x,y,z)
}

class MultiplicationConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  def isSatisfied = {
    trace("Multiplication Constraint on " + varsInfo(x,y,z)){
      val c1 = consistent(z, x.domain * y.domain)
      val c2 = (y.domain == singleton(0)) || consistent(x, z.domain / y.domain)
      val c3 = (x.domain == singleton(0)) || consistent(y, z.domain / x.domain)
      c1 && c2 && c3
    }
  }
  def propogate:List[Var] = { 
    logger.debug("Propogating Multiplication " + varsInfo(x,y,z))
    val changedVars = new ListBuffer[Var]

    if(setVar(z, x.domain * y.domain)) changedVars += z
    if(y.domain != singleton(0) && setVar(x, z.domain / y.domain)) changedVars += x
    if(x.domain != singleton(0) && setVar(y, z.domain / x.domain)) changedVars += y

    logger.debug("Finished Propogating Multiplication")
    return changedVars.toList
  }
  def containsVar(v:Var):Boolean = (v eq x) || (v eq y) || (v eq z)
  override def toString = "Multiplication Constraint " + varNames(x,y,z)
}

class DivisionConstraint(x:Var,y:Var,z:Var)
extends Constraint with Logging {
  def isSatisfied = {
    trace("Division Constraint on " + varsInfo(x,y,z)){
      consistent(z, x.domain / y.domain) &&
      consistent(x, z.domain * y.domain) &&
      consistent(y, x.domain / z.domain)
    }
  }
  def propogate:List[Var] = { 
    logger.debug("Propogating Division " + varsInfo(x,y,z))
    val changedVars = new ListBuffer[Var]

    if(y.domain != singleton(0) && setVar(z, x.domain / y.domain)) changedVars += z
    if(setVar(x, z.domain * y.domain)) changedVars += x
    if(z.domain != singleton(0) && setVar(y, x.domain / z.domain)) changedVars += y

    logger.debug("Finished Propogating Divsion")
    return changedVars.toList
  }
  def containsVar(v:Var):Boolean = (v eq x) || (v eq y) || (v eq z)
  override def toString = "Division Constraint " + varNames(x,y,z)
}

class EqualityConstraint(x:Var, y:Var)
extends Constraint with Logging {
  def isSatisfied = {
    trace("Equality Constraint on " + varsInfo(x,y)){
      consistent(x, y.domain) &&
      consistent(y, x.domain)
    }
  }
  def propogate:List[Var] = { 
    logger.debug("Propogating Equality on " + varsInfo(x,y))
    val intersection = x.domain intersect y.domain
    logger.debug("setVar "+x.name+" to "+intersection)
    logger.debug("setVar "+y.name+" to "+intersection)

    val changedVars = new ListBuffer[Var]
    if(setVar(x, intersection)) changedVars += x
    if(setVar(y, intersection)) changedVars += y

    logger.debug("Finished Propogating Equality")
    return changedVars.toList
  }
  def containsVar(v:Var):Boolean = (v eq x) || (v eq y)
  override def toString = "Equality Constraint " + varNames(x,y)
}

class InEqualityConstraint(x:Var, y:Var)
extends Constraint with Logging {
  def isSatisfied = {
    trace("InEquality Constraint on " + varsInfo(x,y)){
      val intersection = x.domain intersect y.domain
      intersection eq EmptyDomain
    }
  }
  def propogate:List[Var] = {
    val changedVars = new ListBuffer[Var]
    if(x.domain overlap y.domain){
      if(x.isAssigned && !y.isAssigned){
	logger.debug("setVar "+y.name+" to "+(y.domain remove x.domain))
	y := (y.domain remove x.domain)
	changedVars += y
      }
      if(y.isAssigned && !x.isAssigned){
	logger.debug("setVar "+x.name+" to "+(x.domain remove y.domain))
	x := (x.domain remove y.domain)
	changedVars += x
      }
      if(y.isAssigned && x.isAssigned){
	if(x.domain == y.domain){
	  x assign EmptyDomain
	  y assign EmptyDomain
	}
      }
    }
    return changedVars.toList
  }
  def containsVar(v:Var):Boolean = (v eq x) || (v eq y)
  override def toString = "InEquality Constraint " + varNames(x,y)
}

/*
class DifferenceConstraint(vars:List[Var])
extends Constraint with Logging {
  def isSatisfied = {
    trace("Difference Constraints on " + varsInfo(vars:_*)){
      val others = (x:Var) => {
	vars.remove(_ eq x)
      }
      vars.forall(v => 
	others(v).forall(o =>
	  (v.domain intersect o.domain) eq EmptyDomain)) 
    }
  }
  def propogate:List[Var] = {
    logger.debug("Difference Constraint Propogating " + varsInfo(vars:_*))
    val changedVars = new ListBuffer[Var]
    val (assigned,unassigned) = vars.partition(_.isAssigned)
    for(x <- assigned){
      logger.debug(x.name + " is assigned to " + x.domain)
      for(y <- unassigned){
	if(x.domain overlap y.domain){
	  logger.debug(y.name + " = " + y.domain)
	  logger.debug(y.name + " remove " + x.name + " = " + (y.domain remove x.domain))
	  y assign (y.domain remove x.domain)
	  changedVars += y
	  logger.debug(y.name + " = " + y.domain)
	}
      }
    }
    return changedVars.toList
  }
  def containsVar(v:Var):Boolean = vars.exists(_ eq v)
  override def toString = "Difference Constraint " + varNames(vars:_*)
    
}
*/
