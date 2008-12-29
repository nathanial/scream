package org.nrh.scream

trait Constraint extends Logging {
  def satisfy

  def guardConsistent[A](a:Var, b:Domain)(fn: => A) {
    if(!a.isSingleton){
      fn
    }
    else if(a.isSingleton){
      logger.debug("{} is assigned", a.name)
      logger.debug("{}.domain = {}, b = {}", Array(a.name, a.domain.toString, b.toString))
      val intersection = a.domain intersect b    
      logger.debug("comparing {} to {}", a.name, intersection)
      if(a.domain != intersection){
	logger.debug("{} inconsistent with ", a.name, intersection)
	throw new NoSolution(a.name + " inconsistent")
      }
    }
  }

  def constrain[A](a:Var, b:Domain)(fn: => A) {
    guardConsistent(a, b) {
      a := b
      fn
    }
  }

  def allSingleton(vars:Var*):Boolean = vars.forall(_.isSingleton)
}

class NoSolution(msg: String) extends Exception(msg)

class Addition(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy {
    /* z = x + y
     * x = z - y
     * y = z - x */
    logger.debug("Satisfying Addition Constraint")
    if(!allSingleton(x,y,z)){
      constrain(z, x.domain + y.domain){
	logger.debug("{} = {} + {}", Array(z, x, y))
      }
      constrain(x, z.domain - y.domain){
	logger.debug("{} = {} - {}",Array(x,z,y))
      }
      constrain(y, z.domain - x.domain){
	logger.debug("{} = {} - {}",Array(y,z,x))
      }
    }
    logger.debug("Addition Constraint Satisfied")
  }
}

class Subtraction(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy {
    /* z = x - y
     * x = z + y
     * y = x - z */
    logger.debug("Satisfying Subtraction Constraint")
    if(!allSingleton(x,y,z)){
      constrain(z, x.domain - y.domain){
	logger.debug("{} = {} - {}",Array(z,x,y))
      }
      constrain(x, z.domain + y.domain){
	logger.debug("{} = {} + {}",Array(x,z,y))
      }
      constrain(y, x.domain - z.domain){
	logger.debug("{} = {} - {}",Array(y,x,y))
      }
    }
    logger.debug("Constraint Satisfied")
  }
}

class Equality(x:Var, y:Var) extends Constraint {
  def satisfy {
    logger.debug("Satisfying Equality Constraint")
    if(!allSingleton(x,y)){
      val intersection = x.domain intersect y.domain
      constrain(x, intersection) {
	logger.debug("{} == {}",Array(x,y))
      }
      constrain(y, intersection){
	logger.debug("{} == {}",Array(y,x))
      }
    }
    logger.debug("Constraint Satisfied")
  }
}

class Multiplication(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy { 
    /* z = x * y
     * x = z / y
     * y = z / x */
    logger.debug("Satisfying Multiplication Constraint")
    if(!allSingleton(x,y,z)){
      constrain(z, x.domain * y.domain){
	logger.debug("{} = {} * {}",Array(z,x,y))
      }
      constrain(x, z.domain / y.domain){
	logger.debug("{} = {} / {}",Array(x,z,y))
      }
      constrain(y, z.domain / x.domain){
	logger.debug("{} = {} / {}",Array(y,z,x))
      }
    }
    logger.debug("Constraint Satisfied")
  }
}

class Division(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy {
    /* z = x / y
     * x = z * y
     * y = x / z */
    logger.debug("Satisifying Division Constraint")
    if(!allSingleton(x,y,z)){
      constrain(z, x.domain / y.domain){
	logger.debug("{} = {} / {}",Array(z,x,y))
      }
      constrain(x, z.domain * y.domain){
	logger.debug("{} = {} * {}",Array(x,z,y))
      }
      constrain(y, x.domain / z.domain){
	logger.debug("{} = {} / {}",Array(y,x,z))
      }
    }
    logger.debug("Constraint Satisfied")
  }
}



/*class Difference(vars:Var*) extends Constraint {
  def satisfy {
    logger.debug("Satisfying Difference Constraint")

    val (assigned, unassigned) = vars.partition(_.isSingleton)
    for(a <- assigned){
      for(u <- unassigned) {
	constrain(u, u.domain /= a.domain) {
	  logger.debug("{} /= {}", u, a)
	}
      }
    }

    logger.debug("Difference Constraint Satisfied")
  }
}
*/
