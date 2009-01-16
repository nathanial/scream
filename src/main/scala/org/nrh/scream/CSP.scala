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
  def sudoku:Problem = new Problem(new BacktrackingSolver(AC3,MRV))
  def minConflict:Problem = new Problem(new MinConflictsSolver(AC3,50))
}

class CSP {
  private val varBuffer = new ListBuffer[Var]
  private val constraintBuffer = new ListBuffer[Constraint]

  def addVar(nv:Var){
    varBuffer += nv
  }

  def addConstraint(c:Constraint){
    constraintBuffer += c
  }

  def newAnonymousVar:Var = newAnonymousVar(default_domain)

  def newAnonymousVar(domain:Domain):Var = {
    val nv = new Var(this,false)
    nv.domainStack.push(domain)
    nv.name = "anonymous"
    varBuffer += nv
    return nv
  }

  def vars = varBuffer.toList
  def constraints = constraintBuffer.toList
  def userVars = vars.filter(_.isFromUser)
  def allSatisfied = vars.forall(_.isSatisfied)
  def unsatisfied = vars.filter(!_.isSatisfied)
  def assigned = vars.filter(_.isAssigned)
  def unassigned = vars.filter(!_.isAssigned)
  def allAssigned = vars.forall(_.isAssigned)
  def isConsistent = vars.forall(_.isConsistent)
  def isSolved = allSatisfied && allAssigned
  def unsatisfiedConstraints = constraints.filter(!_.isSatisfied)
  def conflicted = vars.filter(v => !v.isSatisfied && v.isConsistent)
  def pushNewDomains { vars.foreach(v => v.domainStack.push(v.domain.shallowCopy)) }
  def popDomains { vars.foreach(v => v.domainStack.pop) }
 
 override def toString:String = {
    userVars.map(v => {
      "("+v.name+" -> "+v.domain+")"
    }).mkString(" ")
  }

  def compare(that:CSP) = {
    val len1 = this.assigned.length
    val len2 = that.assigned.length
    if(len1 > len2) 1
    else if(len1 == len2) 0
    else -1
  }

  def bind[A](fn: => A):A = {
    pushNewDomains
    val result = fn
    popDomains
    return result
  }

}


    
