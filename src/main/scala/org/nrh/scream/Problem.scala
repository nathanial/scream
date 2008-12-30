package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Domain._
import org.nrh.scream.Range._

class Problem extends Logging {
  val constraints = new ListBuffer[Constraint]()
  val vars = new ListBuffer[Var]()
  var varCounter = 0

  def newVar(name: String, domain: Domain):Var = {
    val nv = new DomainVar(varCounter, name, domain, this)
    varCounter += 1
    vars += nv
    return nv
  }

  def newVar:Var = newVar("default")
  def newVar(name: String):Var = newVar(name, domain(0 upto 10000))

  def addConstraint(c:Constraint) {
    constraints += c
  }

  def apply(name:String):Domain = {
    vars.find(v => v.name == name) match {
      case Some(x) => x.domain
      case None =>
	throw new IllegalArgumentException(
	  "var with name " + name + " does not exist")
    }
  }

  def propogateConstraints {
    var changed = true
    while(changed){
      changed = false
      constraints.foreach(_.satisfy)
      if(vars.exists(_.changed)){
	vars.foreach(v => v.changed = false)
	changed = true
      }
    }
  }

  def findSolution {
    var root = new Node(vars.map(v => new VarState(v.id, v.name, v.domain)).toList)
    logger.debug("Root = " + root)
    val solution = findSolution(root)
    if(solution == null){
      throw new NoSolution("solution not found")
    }
    else{
      logger.debug("Solution = " + solution)
    }
  }

  def findSolution(node: Node):Node = {
    if(node.isLeaf){
      logger.debug("Leaf = " + node)
      for(s <- node.states){
	val v = vars.find(_.id == s.id).get
	v assign s.domain
      }
      try{
	propogateConstraints
	return node
      } catch {
	case (e: NoSolution) => {
	  logger.debug("No Solution at " + node)
	  return null
	}
      }	  
    }
    else {
      logger.debug("Node = " + node)
      val ss = node.successors.elements
      var found = false
      var result:Node = null
      while(!found && ss.hasNext){
	val s = ss.next
	result = findSolution(s)
	if(result != null){
	  found = true
	}
      }
      return result
    }
  }
}

class Node(val states: List[VarState]){
  def successors:List[Node] = {
    val unassigned = states.filter(x => !x.isAssigned)
    if(!unassigned.isEmpty){
      val child = unassigned.first
      val permutations = child.domain.map(x => new VarState(child.id, child.name, x))
      val otherstates = states.filter(_.id != child.id)
      val children = permutations.map(x => new Node(x :: otherstates))
      return children.toList
    }
    else{
      return Nil
    }
  }

  override def toString = "(Node " + states.mkString(",") + ")"

  def isLeaf:Boolean = states.forall(_.isAssigned)
}

case class VarState(id: Int, name:String, domain: Domain) {
  def isAssigned:Boolean = domain.isSingleton
  override def toString = "(VS "+ id + ", "+name+","+domain+")"
}

    
    

