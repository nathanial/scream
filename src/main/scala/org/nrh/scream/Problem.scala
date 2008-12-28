package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Domain._

class Problem {
  val constraints = new ListBuffer[Constraint]()
  val vars = new ListBuffer[Var]()

  def newVar(name: String, domain: Domain):Var = {
    val nv = new DomainVar(name, this, domain)
    vars += nv
    return nv
  }
  def newVar:Var = newVar("default")
  def newVar(name: String):Var = newVar(name, domain(0,10000))

  def addConstraint(c:Constraint) {
    constraints += c
  }

  def solve { 
    propogate
/*    if(!vars.forall(_.isAssigned)){
      search
    }
*/
  }

  def apply(name:String):Domain = {
    vars.find(v => v.name == name) match {
      case Some(x) => x.domain
      case None =>
	throw new IllegalArgumentException(
	  "var with name " + name + " does not exist")
    }
  }

  def propogate {
    var change = true
    while(change){
      change = false
      constraints.foreach(_.satisfy)
      if(vars.exists(_.changed)){
	vars.foreach(v => v.changed = false)
	change = true
      }
    }
  }
}
