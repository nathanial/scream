package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

class Problem extends Logging {
  val state = State.newState

  def newVar:Var = {
    val nv = Var.newVar(state)
    nv.name = "default"
    return nv
  }

  def newVar(name: String):Var = {
    val nv = Var.newVar(domain(interval(0,10000000)), state,true)
    nv.name = name
    return nv
  }

  def newVar(name:String,domain:Domain):Var = {
    val nv = Var.newVar(domain,state,true)
    nv.name = name
    return nv
  }

  def propogateConstraints { Solver.propogateConstraints(state) }
  def findSolution:State = Solver.findSolution(state)
  def addConstraint(c:Constraint) {
    state.add(c)
  }
}
