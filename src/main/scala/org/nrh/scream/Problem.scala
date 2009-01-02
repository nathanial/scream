package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

class Problem extends Logging {
  val state = State.newState

  def newVar(name: String, domain: Domain):Var = {
    val nv = new DomainVar
    nv.name = name
    state.set(nv,new VarState(domain))
    return nv
  }

  def newVar:Var = newVar("default")
  def newVar(name: String):Var = newVar(name, domain(interval(0,10000)))

  def propogateConstraints { Solver.propogateConstraints(state) }
  def findSolution:State = Solver.findSolution(state)
  def addConstraint(c:Constraint) {
    state.add(c)
  }
}
