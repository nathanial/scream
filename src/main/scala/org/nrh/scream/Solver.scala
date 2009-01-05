package org.nrh.scream
import scala.collection.mutable.{ListBuffer,Buffer,HashMap,Map,Queue}
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._

object Solver extends Logging {
  def propogateConstraints(_state:State) {
    implicit val state = _state
    val varQueue = new Queue[Var]
    varQueue ++= state.vars
    while(!varQueue.isEmpty){
      val v = varQueue.dequeue
//      logger.debug("v = " + v)
      for(c <- v.constraints){
	if(!c.isSatisfied){
	  val changed = c.propogate
//	  logger.debug("neighbors = " + neighbors.mkString(" "))
	  changed.foreach(x => varQueue.enqueue(x))
	}
      }
    }
  }

  def findSolution(state:State):State = {
    var root = new Node(state)
    propogateConstraints(state)
    logger.debug("Root = " + root)
    val solution = findSolution(root, 1)
    if(solution == null){
      throw new NoSolution("solution not found")
    }
    else{
      logger.debug("Solution = " + solution)
      return solution.state
    }
  }

  def findSolution(node: Node, depth:Int):Node = {
    implicit val state = node.state
    if(node.isLeaf){
      logger.debug("Leaf = " + node)
      if(state.allSatisfied){
	return node
      }
      else {
	logger.debug("No Solution at depth "+depth+" = " + node)
	val unsatisfied = state.unsatisfied.toList
	logger.debug("Because")
	for(uv <- unsatisfied){
	  println("Var = " + uv.name +" "+uv.domain)
	  println("Constraints = " + uv.myState.constraints.remove(_.isSatisfied).toList)
	}
	return null
      }
    }
    else {
      logger.debug("Node at depth "+depth+" = "+node) 
      val ss = node.successors.elements
      var result:Node = null
      while(result == null && ss.hasNext){
	val node = ss.next
	val state = node.state
	propogateConstraints(state)
	if(!state.vars.exists(v => v.domain(state) eq EmptyDomain)){
	  result = findSolution(node,depth + 1)
	}
      }
      return result
    }
  }
}

class Node(val state:State) extends Logging {
  def successors:List[Node] = {
    val unlist = state.nextUnAssigned
    logger.debug("Choose " + unlist)
    unlist match {
      case None => Nil
      case Some(v) => {
	val permutations = v.domain(state) map { 
	  x => state.mimicWith(v, v.myState(state).mimicWith(singleton(x)))
	}
	val children = permutations.map(x => new Node(x))
	return children.toList
      }
    }
  }

  override def toString = {
    implicit val _state = state
    val varToString = (v:Var) => {
      "("+v.name+" "+v.domain+")"
    }
    "(Node " + state.userVars.map(varToString).mkString(" ") + ")"
  }
    

  def isLeaf:Boolean = state.varStates.values.forall(_.isAssigned)
}

class NoSolution(msg:String) extends Exception(msg)
