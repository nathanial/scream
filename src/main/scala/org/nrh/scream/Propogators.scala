package org.nrh.scream

trait Propogator {
  def propogate(implicit state:State)
  def setVar(x:Var, y:Domain)(implicit state: State){
    if(!x.isAssigned){
      x := y
    }
  }
}

class AdditionPropogator(x:Var,y:Var,z:Var) extends Propogator with Logging {
  def propogate(implicit state:State) { 
    logger.debug("Propogating Addition on " + Array(x,y,z).mkString(" "))
    logger.debug("Staring domains = " + Array(x,y,z).map(_.domain).mkString(" "))    
    setVar(z, x.domain + y.domain)
    setVar(x, z.domain - y.domain)
    setVar(y, z.domain - x.domain)
    logger.debug("Finished Propogating Addition")
  }
}
class SubtractionPropogator(x:Var,y:Var,z:Var) extends Propogator with Logging {
  def propogate(implicit state:State) { 
    logger.debug("Propogating Subtraction on " + Array(x,y,z).mkString(" "))
    setVar(z, x.domain - y.domain)
    setVar(x, z.domain + y.domain)
    setVar(y, x.domain - z.domain)
    logger.debug("Finished Propogating Subtraction")
  }
}
class MultiplicationPropogator(x:Var,y:Var,z:Var) extends Propogator with Logging {
  def propogate(implicit state:State) { 
    logger.debug("Propogating Multiplication " + Array(x,y,z).mkString(" "))
    setVar(z, x.domain * y.domain)
    setVar(x, z.domain / y.domain)
    setVar(y, z.domain / x.domain)
    logger.debug("Finished Propogating Multiplication")
  }
}
class DivisionPropogator(x:Var,y:Var,z:Var) extends Propogator with Logging {
  def propogate(implicit state:State) { 
    logger.debug("Propogating Division " + Array(x,y,z).mkString(" "))
    setVar(z, x.domain / y.domain)
    setVar(x, z.domain * y.domain)
    setVar(y, x.domain / z.domain)
    logger.debug("Finished Propogating Divsion")
  }
}
class EqualityPropogator(x:Var,y:Var) extends Propogator with Logging {
  def propogate(implicit state:State) { 
    logger.debug("Propogating Equality on {} {}",x,y)
    val intersection = x.domain intersect y.domain
    setVar(x, intersection)
    setVar(y, intersection)
    logger.debug("Finished Propogating Equality")
  }
}
class NonPropogator extends Propogator {
  def propogate(implicit state:State) { 
    null
  }
}
