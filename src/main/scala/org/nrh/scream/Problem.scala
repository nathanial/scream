package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map

class Problem {
  val constraintStore = new ConstraintStore  
  val vars = new ListBuffer[Variable]()

  def newVar(domain: Domain): Variable = {
    val nv = new Variable(domain, this)
    vars += nv
    return nv
  }
  
  def newVar:Variable = newVar(new Domain(new Range(0, 10000)))

  def label(lvars: Map[String, Variable]):Map[String, Range] = {
    var changed = true
    while(changed) {
      changed = false
      constraintStore.propogate
      if(vars.exists(_.isChanged)){
	changed = true
      }
    }      
    
    return lvars mapElements { _.domain.range }
  }

  def constrain(cs: Constraint*) {
    for(c <- cs)
      this += c
  }

  def +=(c: Constraint){
    constraintStore += c
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
    

class Variable(var domain: Domain, val problem: Problem) {
  private var changed = false

  def +(that: Variable):Variable = {
    val nv = problem.newVar
    val constraint = new AdditionConstraint(this,that, nv)
    problem += constraint
    return nv
  }    

  def -(that: Variable):Variable = {
    val nv = problem.newVar
    val constraint = new SubtractionConstraint(this, that, nv)
    problem += constraint
    return nv
  }

  def /(that: Variable):Variable = {
    val nv = problem.newVar
    val constraint = new DivisionConstraint(this, that, nv)
    problem += constraint
    return nv
  }

  def *(that: Variable):Variable = {
    val nv = problem.newVar
    val constraint = new MultiplicationConstraint(this, that, nv)
    problem += constraint
    return nv
  }

  def ==(that: Variable):Constraint = {
    val intersection = this.domain intersect that.domain
    return new EqualityConstraint(this, that)
  }

  def ==(that: BigInt):Constraint = {
    val nv = problem.newVar(new Domain(new Range(that, that)))
    return this.==(nv)
  }
  
  def update(d: Domain) {
    if(!(this.domain equals d)){
      println("odomain = " + this.domain.range)
      println("pdomain = " + d.range)
      val ndomain = this.domain - d
      println("ndomain = " + ndomain.range)
      if(!(this.domain equals ndomain)){
	changed = true
	this.domain = ndomain
      }
    }
  }

  def isChanged:Boolean = {
    if(changed){
      changed = false
      return true
    }
    else{
      return false
    }
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

  def equals(that:Domain):Boolean = {
    if(!(this.range.min equals that.range.min)){
      return false
    }
    if(!(this.range.max equals that.range.max)){
      return false
    }
    return true
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
      var nmin:BigInt = null
      var nmax:BigInt = null
  
      if(that.max == 0){
	nmin = this.min
      }
      else{
	nmin = this.min / that.max
      }

      if(that.min == 0){
	nmax = this.max
      }
      else{
	nmax = this.max / that.min
      }

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

  def equals(that:Range):Boolean = {
    if((this.min equals that.min) && (this.max equals that.max)){
      return true
    }
    return false
  }

  def equals(that:Int):Boolean = {
    if(this.min equals this.max){
      if(this.min equals that){
	return true
      }
    }
    return false
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
