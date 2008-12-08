package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map

class Problem {
  val constraintStore = new ConstraintStore  
  val vars = new ListBuffer[Var]()

  def newVar(domain: Domain): Var = {
    val nv = new DomainVar(domain)
    vars += nv
    return nv
  }
  
  def newVar:Var = newVar(new Domain(new Range(0, 10000)))

  def label(lvars: Map[String, Var]):Map[String, Range] = null

  def constrain(vars: Var*) {
    for(v <- vars)
      this += v
  }

  def +=(v: Var){
    null
  }
    
}

class ConstraintStore {
  val constraints = new ListBuffer[Constraint]()

  def propogate {
    if(constraints.map(_.satisfy).exists(_ == false)){
      throw new NoSolution()
    }
  }

  def +=(c: Constraint) {
    constraints += c
  }
}
    
class Domain(val range: Range) {
  import org.nrh.scream.Domain._

  def checkEmpty[A](domains: Domain*)(fn: => A):A = {
    if(domains.exists(_.isEmpty)){
      throw new DomainException("empty domains")
    }
    else{
      return fn
    }
  }

  def ==(that:Domain):Boolean = {
    if(that == null){
      return false
    }
    else if(this.getClass != that.getClass){
      return false
    }
    else if(!(this.range.min == that.range.min)){
      return false
    } 
    else if (!(this.range.max == that.range.max)){
      return false
    }
    else {
      return true
    }
  }

  def isEmpty:Boolean = range match {
    case (a:Empty) => true
    case _ => false
  }    
  
  def intersect(that:Domain):Domain = {
    new Domain(this.range intersect that.range)
  }

  def -(that:Domain):Domain = {
    val nmin = if(contains(that.min)) that.min else this.min
    val nmax = if(contains(that.max)) that.max else this.max
    return domain(nmin, nmax)
  }

  def contains(num:BigInt):Boolean = 
    (this.min <= num) && (num <= this.max)
    
  def arithPlus(that:Domain):Domain = {
    checkEmpty(this, that){
      return domain(this.min + that.min, this.max + that.max)
    }
  }

  def arithMinus(that:Domain):Domain = {
    checkEmpty(this, that){
      var nmin = this.min - that.max
      var nmax = this.max - that.min
      if(nmin < 0) { nmin = 0 }
      if(nmax < 0) { nmax = 0 }
      return domain(nmin, nmax)
    }
  }

  def arithMultiply(that:Domain):Domain = {
    checkEmpty(this, that) {
      return domain(this.min * that.min, this.max * that.max)
    }
  }

  def arithDivide(that:Domain):Domain = {
    //min = this.min / that.max
    //max = this.max / that.min

    checkEmpty(this, that) {
      val nmin = if(that.max == 0) this.min else { this.min / that.max }
      val nmax = if(that.min == 0) this.max else { this.max / that.min }
      return domain(nmin, nmax)
    }
  }

  def min:BigInt = range.min
 
  def max:BigInt = range.max
}

class DomainException(val msg: String) extends Exception

class Range(val min: BigInt, val max: BigInt){
  def intersect(that:Range):Range = {
    var nmin = this.min max that.min
    var nmax = this.max min that.max
    if(nmin < 0) { nmin = 0 }
    if(nmax < 0) { nmax = 0 }
    val nrange = new Range(nmin, nmax)
    return nrange
  }

  override def toString = "Range(min = " + min + ", max = " + max + ")"

  def ==(that:Range):Boolean = {
    if(that == null){
      return false
    }
    else if(this.getClass != that.getClass){
      return false
    }
    else if(!((this.min == that.min) && (this.max == that.max))){
      return false
    }
    else {
      return true
    }
  }

}

class Empty extends Range(null,null)

object Domain {
  def domain(range: Range): Domain = {
    return new Domain(range)
  }

  def domain(min: BigInt, max: BigInt): Domain = {
    return new Domain(new Range(min, max))
  }

  def domain(num: BigInt):Domain = {
    return new Domain(new Range(num,num))
  }
}

class NoSolution extends Exception

