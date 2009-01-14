package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

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
    val c = new DifferenceConstraint(vars.toList)
    for(v <- vars)
      v constrain c
  }

  def allDiff(vars:List[Var]){
    val c = new DifferenceConstraint(vars)
    for(v <- vars)
      v constrain c
  }

  def propogateConstraints {
    solver.propogateConstraints(csp)
  }

  def findSolution {
    solver.findSolution(csp) match {
      case None => throw new RuntimeException("No Solution Found")
      case Some(s) => s.assignToVars
    }
  }
}

object Problem {
  def standard:Problem = new Problem(new BacktrackingSolver(AC3,MRV))
  def sudoku:Problem = standard
}

class CSP {
  val vars = new ListBuffer[Var]

  def addVar(nv:Var){
    vars += nv
  }

  def newAnonymousVar:Var = newAnonymousVar(default_domain)

  def newAnonymousVar(domain:Domain):Var = {
    val nv = new Var(this,false)
    nv.domainStack.push(domain)
    nv.name = "anonymous"
    vars += nv
    return nv
  }

  def userVars = vars.filter(_.isFromUser)
  def allSatisfied = vars.forall(_.isSatisfied)
  def unsatisfied = vars.filter(!_.isSatisfied)
  def assigned = vars.filter(_.isAssigned).toList
  def unassigned = vars.filter(!_.isAssigned)
  def allAssigned = vars.forall(_.isAssigned)
  def consistent = !vars.exists(_.domain eq EmptyDomain)
  def isConsistent = consistent
  def isSolution = allSatisfied && allAssigned
  def unsatisfiedConstraints = vars.flatMap(_.constraints).filter(!_.isSatisfied).toList
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
}

object CSP {
  def bind[A](csp:CSP)(fn: => A):A = {
    csp.pushNewDomains
    val result = fn
    csp.popDomains
    return result
  }
}
    
