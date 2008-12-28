package org.nrh.scream
import org.nrh.scream.Debug._

trait Constraint {
  def satisfy

  def guardConsistent[A](a:Var, b:Domain)(fn: => A) {
    if(!a.isAssigned){
      fn
    }
    else if(a.isAssigned){
      debug(a.name + " is assigned")
      println("a.domain = " + a.domain + ", b = " + b)
      val intersection = a.domain intersect b    
      debug("comparing " + a.name + " to " + intersection)
      if(a.domain != intersection){
	debug(a.name + "("+a.domain+") inconsistent with " + intersection)
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

  def allAssigned(vars:Var*):Boolean = vars.forall(_.isAssigned)
}

class NoSolution(msg: String) extends Exception(msg)

class Addition(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy {
    /* z = x + y
     * x = z - y
     * y = z - x */
    debug("Satisfying Addition Constraint")
    if(!allAssigned(x,y,z)){
      constrain(z, x.domain + y.domain){
	debug("%s = %s + %s", z, x, y)
      }
      constrain(x, z.domain - y.domain){
	debug("%s = %s - %s",x,z,y)
      }
      constrain(y, z.domain - x.domain){
	debug("%s = %s - %s",y,z,x)
      }
    }
    debug("Addition Constraint Satisfied")
  }
}

class Subtraction(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy {
    /* z = x - y
     * x = z + y
     * y = x - z */
    debug("Satisfying Subtraction Constraint")
    if(!allAssigned(x,y,z)){
      constrain(z, x.domain - y.domain){
	debug("%s = %s - %s",z,x,y)
      }
      constrain(x, z.domain + y.domain){
	debug("%s = %s + %s",x,z,y)
      }
      constrain(y, x.domain - z.domain){
	debug("%s = %s - %s",y,x,y)
      }
    }
    debug("Constraint Satisfied")
  }
}

class Equality(x:Var, y:Var) extends Constraint {
  def satisfy {
    debug("Satisfying Equality Constraint")
    if(!allAssigned(x,y)){
      val intersection = x.domain intersect y.domain
      constrain(x, intersection) {
	debug("%s == %s",x,y)
      }
      constrain(y, intersection){
	debug("%s == %s",y,x)
      }
    }
    debug("Constraint Satisfied")
  }
}

class Multiplication(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy { 
    /* z = x * y
     * x = z / y
     * y = z / x */
    debug("Satisfying Multiplication Constraint")
    if(!allAssigned(x,y,z)){
      constrain(z, x.domain * y.domain){
	debug("%s = %s * %s",z,x,y)
      }
      constrain(x, z.domain / y.domain){
	debug("%s = %s / %s",x,z,y)
      }
      constrain(y, z.domain / x.domain){
	debug("%s = %s / %s",y,z,x)
      }
    }
    debug("Constraint Satisfied")
  }
}

class Division(x:Var, y:Var, z:Var) extends Constraint {
  def satisfy {
    /* z = x / y
     * x = z * y
     * y = x / z */
    debug("Satisifying Division Constraint")
    if(!allAssigned(x,y,z)){
      constrain(z, x.domain / y.domain){
	debug("%s = %s / %s",z,x,y)
      }
      constrain(x, z.domain * y.domain){
	debug("%s = %s * %s",x,z,y)
      }
      constrain(y, x.domain / z.domain){
	debug("%s = %s / %s",y,x,z)
      }
    }
    debug("Constraint Satisfied")
  }
}


/*class Difference(vars:Var*) extends Constraint {
  def satisfy {
    debug("Satisfying Difference Constraint")

    val (assigned, unassigned) = vars.partition(_.isAssigned)
    for(a <- assigned){
      for(u <- unassigned) {
	constrain(u, u.domain /= a.domain) {
	  debug("%s /= %s", u, a)
	}
      }
    }

    debug("Difference Constraint Satisfied")
  }
}
*/
