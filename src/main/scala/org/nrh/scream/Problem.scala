package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._ 
import org.nrh.scream.Util._
import org.nrh.scream.Profiler._

class Problem(val solver:Solver) { 
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
    allDiff(vars.toList)
  }

  def allDiff(vars:List[Var]){
    val others = Util.others(vars)_
    for(x <- vars; y <- others(x)){
      csp.addConstraint(new InEqualityConstraint(x,y))
    }
  }

  def propogateConstraints {
    solver.propogate(csp.vars)
  }

  def firstSolution { 
    timed('firstSolution){
      solver.firstSolution(csp) match {
	case None => throw new RuntimeException("No Solution Found")
	case Some(s) => s.assignToVars
      }
    }
  }

  def allSolutions:Iterator[Option[Solution]] = solver.allSolutions(csp)
}

object Problem {
  def stdBTConfig(depth:Int, elementsExtractor: Domain => Iterator[BigInt]) = 
  {
    new BacktrackingSolverConfiguration(AC3,MRV,depth,elementsExtractor)
  }

  def standard:Problem =
    new Problem(new BacktrackingSolver(stdBTConfig(Math.MAX_INT, _.elements)))

  def randomized:Problem = 
    new Problem(
      new BacktrackingSolver(
	stdBTConfig(Math.MAX_INT,_.randomizedElements)))

  def randomized(depth:Int) =
    new Problem(new BacktrackingSolver(
      stdBTConfig(depth,_.randomizedElements)))

  def standard(depth:Int) = 
    new Problem(new BacktrackingSolver(
      stdBTConfig(depth,_.elements)))
}
