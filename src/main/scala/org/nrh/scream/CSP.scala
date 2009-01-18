package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._ 
import org.nrh.scream.Util._

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


    
