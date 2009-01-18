package org.nrh.scream
import scala.collection.mutable.{Queue,Stack,ListBuffer,HashMap}
import scala.util.Random
import org.nrh.scream.Domain._
import org.nrh.scream.Interval._  
import org.nrh.scream.Util._
import org.nrh.scream.Solution._

trait Solver
{
  def firstSolution(csp:CSP):Option[Solution]
  def allSolutions(csp:CSP):Iterator[Option[Solution]]
  def propogate(csp:CSP)
}

class State(val assignments:Map[Var,Domain]){
  def assignToVars {
    assignments.foreach(item => item._1 assign item._2)
  }
}  

class Solution(a:Map[Var,Domain]) extends State(a)

object State {
  def apply(vars:Seq[Var]):State = {
    new State(Map(vars.map(v => (v,v.domain)):_*))
  }
}

object Solution {
  def solution(vars:Seq[Var]):Solution = {
    new Solution(Map(vars.map(v => (v,v.domain)):_*))
  }
}

class Assignment(val variable:Var, val assignments:Iterator[BigInt])

object Assignment {
  def apply(v:Var, nums:Iterator[BigInt]):Assignment = {
    new Assignment(v,nums)
  }
}
  
