package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer}
import scala.collection.mutable.{HashMap,Map}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  

object Solver extends Logging {
  def propogateConstraints(_state:State) {
    implicit val state = _state
    val vars = state.varStates.keys
    var changed = true
    while(changed){
      changed = false
      state.constraints.filter(!_.isSatisfied).foreach(_.propogator.propogate)
      if(vars.exists(_.changed)){
	vars.foreach(v => v.setChanged(false))
	changed = true
      }
    }
  }

  def findSolution(state:State):State = {
    var root = new Node(state)
    logger.debug("Root = " + root)
    val solution = findSolution(root)
    if(solution == null){
      throw new NoSolution("solution not found")
    }
    else{
      logger.debug("Solution = " + solution)
      return solution.state
    }
  }

  def findSolution(node: Node):Node = {
    implicit val state = node.state
    if(node.isLeaf){
      logger.debug("Leaf = " + node)
      if(state.constraints.forall(_.isSatisfied)){
	return node
      }
      else {
	logger.debug("No Solution = " + node)
	return null
      }
    }
    else {
      logger.debug("Node = " + node)      
      logger.debug("State = " + state.varStates)
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

class Node(val state:State){
  def successors:List[Node] = {
    val unassigned = state.varStates.find(item => {
      val (v,vst) = item
      !vst.isAssigned
    })
    unassigned match {
      case None => Nil
      case Some((v,vst)) => {
	val permutations = vst.domain.map(x => 
	  state.mimicWith(v, new VarState(domain(interval(x,x)))))
	permutations.foreach(p => Solver.propogateConstraints(p))
	val children = permutations.map(x => new Node(x))
	return children.toList
      }
    }
  }

  override def toString = "(Node " + state.varStates + ")"

  def isLeaf:Boolean = state.varStates.values.forall(_.isAssigned)
}

class NoSolution(msg:String) extends Exception(msg)
