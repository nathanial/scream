package org.nrh.scream

trait Constraint {
  def satisfy:Boolean
}

trait UnaryConstraint extends Constraint

trait BinaryConstraint extends Constraint

trait ConstraintException extends Exception
  
