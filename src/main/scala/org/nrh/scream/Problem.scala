package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._ 
import org.nrh.scream.Util._

class Problem(solver:Solver) { 
  val csp = new CSP

  def newVar:Var = newVar("default")

  def newVar(name: String):Var = newVar(name, default_domain)

  def newVar(name:String,domain:Domain):Var = {
    val nv = new Var(csp,true)
    nv.domainStack.push(domain)
    nv.name = name
    csp.addVar(nv)
    return nv
  }

  def allDiff(vars:Var*){
    csp.addConstraint(new DifferenceConstraint(vars.toList))
  }

  def allDiff(vars:List[Var]){
    csp.addConstraint(new DifferenceConstraint(vars))
  }

  def propogateConstraints {
    solver.propogateConstraints(csp)
  }

  def firstSolution {
    solver.firstSolution(csp) match {
      case None => throw new RuntimeException("No Solution Found")
      case Some(s) => s.assignToVars
    }
  }

  def allSolutions:Iterator[Option[Solution]] = solver.allSolutions(csp)
}

object Problem {
  def standard:Problem = new Problem(new BacktrackingSolver(AC3,MRV))
  def minConflict:Problem = new Problem(new MinConflictsSolver(AC3,50))
}
