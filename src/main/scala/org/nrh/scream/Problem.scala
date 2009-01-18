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
    allDiff(vars.toList)
  }

  def allDiff(vars:List[Var]){
    val others = Util.others(vars)_
    for(x <- vars; y <- others(x)){
      csp.addConstraint(new InEqualityConstraint(x,y))
    }
  }

  def propogateConstraints {
    solver.propogate(csp)
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
  def stdBTConfig(depth:Int, 
		  elementsExtractor: Domain => Iterator[BigInt],
		  searchCallback: State => Unit) =
  {
    new BacktrackingSolverConfiguration(AC3,MRV,depth,elementsExtractor, searchCallback)
  }

  def standard:Problem =
    new Problem(new BacktrackingSolver(stdBTConfig(Math.MAX_INT, _.elements, s => {})))

  def randomized:Problem = 
    new Problem(
      new BacktrackingSolver(
	stdBTConfig(Math.MAX_INT,_.randomizedElements, s => {})))

  def randomized(depth:Int) =
    new Problem(new BacktrackingSolver(
      stdBTConfig(depth,_.randomizedElements, s => {})))

  def standard(depth:Int) = 
    new Problem(new BacktrackingSolver(
      stdBTConfig(depth,_.elements, s => {})))
      
  def standard(depth:Int, searchCallback: State => Unit) = 
    new Problem(new BacktrackingSolver(
      stdBTConfig(depth,_.elements, searchCallback)))
      
  def randomized(depth:Int, searchCallback: State => Unit) =
    new Problem(new BacktrackingSolver(
      stdBTConfig(depth,_.randomizedElements, searchCallback)))

 
}
