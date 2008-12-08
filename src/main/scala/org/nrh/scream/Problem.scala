package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Domain._

class Problem {
  val constraintStore = new ConstraintStore  
  val vars = new ListBuffer[Var]()

  def newVar(domain: Domain): Var = {
    val nv = new DomainVar(domain)
    vars += nv
    return nv
  }
  
  def newVar:Var = newVar(domain(0, 10000))

  def label(lvars: Map[String, Var]):Map[String, Range] = null

  def constrain(vars: Var*) {
    for(v <- vars)
      this += v
  }

  def +=(v: Var){
    null
  }
    
}

class ConstraintStore {
  val constraints = new ListBuffer[Constraint]()

  def propogate {
    if(constraints.map(_.satisfy).exists(_ == false)){
      throw new NoSolution()
    }
  }

  def +=(c: Constraint) {
    constraints += c
  }
}
    
class NoSolution extends Exception

