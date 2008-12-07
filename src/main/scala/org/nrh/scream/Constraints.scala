package org.nrh.scream

trait Constraint {
  def satisfy:Boolean
}

class AdditionConstraint(x: Variable, y: Variable, z: Variable) extends Constraint {
  def satisfy: Boolean = {
    println("Satisfying AdditionConstraint")
    //x + y = z

    try {
      println("assigning z = x + y")
      z update { x.domain arithPlus y.domain }
      println("assigning x = z - y")
      x update { z.domain arithMinus y.domain }
      println("assigning y = z - x")
      y update { z.domain arithMinus x.domain }
      return true
    }catch {
      case (e:DomainException) => return false
    }
  }
}

class SubtractionConstraint(x: Variable, y: Variable, z: Variable) extends Constraint {
  def satisfy: Boolean = {
    println("Satisfying Subtraction Constraint")
    //x - y = z
    
    try {
      println("assigning z = x - y")
      z update { x.domain arithMinus y.domain }
      println("assigning x = z + y")
      x update { z.domain arithPlus y.domain }
      println("assigning y = z + x")
      y update { z.domain arithPlus x.domain }
      return true
    } catch {
      case (e:DomainException) => return false
    }
  }
}

oclass EqualityConstraint(x: Variable, y: Variable) extends Constraint {
  def satisfy: Boolean = {
    println("Satisfying EqualityConstraint")
    //x == y
    val intersection = x.domain intersect y.domain
    try {
      x update intersection
      y update intersection
      return true
    } catch {
      case (e:DomainException) => return false
    }
  }
}

class MultiplicationConstraint(x: Variable, y: Variable, z: Variable) extends Constraint {
  def satisfy: Boolean = {
    println("Satisfying MultiplicationConstraint")
    
    try {
      println("assigning z = x * y")
      z update { x.domain arithMultiply y.domain }
      println("assigning x = z / y")
      x update { z.domain arithDivide y.domain }
      println("assigning y = z / x")
      y update { z.domain arithDivide x.domain }
      return true
    } catch {
      case (e:DomainException) => return false
    }
  }
}
 
class DivisionConstraint(x: Variable, y: Variable, z: Variable) extends Constraint {
  def satisfy: Boolean = {
    println("Satisfying DivisionConstraint")
    
    try {
      println("assigning z = x / y")
      z update { x.domain arithDivide y.domain }
      println("assigning x = z * y")
      x update { z.domain arithMultiply y.domain }
      println("assigning y = z * x")
      y update { z.domain arithMultiply x.domain }
      return true
    } catch {
      case (e:DomainException) => return false
    }
  }
}
    
