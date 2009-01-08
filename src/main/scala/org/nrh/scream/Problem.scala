package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

class Problem extends Logging {
  val state = State.newState
  val solver = new BacktrackingSolver(AC3, MRV)

  def newVar:Var = {
    val nv = Var.newVar(state)
    nv.name = "default"
    return nv
  }

  def newVar(name: String):Var = {
    val nv = Var.newVar(state, domain(interval(0,10000000)), true, Nil)
    nv.name = name
    return nv
  }

  def newVar(name:String,domain:Domain):Var = {
    val nv = Var.newVar(state,domain,true, Nil)
    nv.name = name
    return nv
  }

  def allDiff(vars:Var*){
    implicit val s = state
    val c = new DifferenceConstraint(vars.toList)
    for(v <- vars)
      v constrain c
  }

  def allDiff(vars:List[Var]){
    implicit val s = state
    val c = new DifferenceConstraint(vars)
    for(v <- vars)
      v constrain c
  }


  def propogateConstraints { AC3(state) }
  def findSolution:Option[State] = solver(state)
}


