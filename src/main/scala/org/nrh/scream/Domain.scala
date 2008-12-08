package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map

trait Domain {
  def intersection(that:Domain):Domain
  def union(that:Domain):Domain
  def min:BigInt
  def max:BigInt
  def range:Range
}

class IntervalDomain(val range: Range) extends Domain {
  import org.nrh.scream.Domain._

  override def equals(t1:Any):Boolean = {
    if(!t1.isInstanceOf[AnyRef]){
      return false
    }
    else{
      val t2 = t1.asInstanceOf[AnyRef]   
      if(this.getClass != t2.getClass){
	return false
      }
      else {
	val t3 = t1.asInstanceOf[IntervalDomain]
	if(!(this.range.min == t3.range.min)){
	  return false
	} 
	else if (!(this.range.max == t3.range.max)){
	  return false
	}
	else {
	  return true
	}
      }
    }
  }

  def intersection(that:Domain):Domain = {    
    (this.range intersection that.range) match {
      case Some(r) => domain(r)
      case None => Empty
    }
  }

  def union(that:Domain):Domain = {
    domain(this.range union that.range)
  }

  def contains(num:BigInt):Boolean = 
    (this.min <= num) && (num <= this.max)
    
  def min:BigInt = range.min
 
  def max:BigInt = range.max

  override def toString:String = this.range.toString
}

object Empty extends Domain {
  def intersection(that:Domain):Domain = Empty
  def union(that:Domain):Domain = that
  def min:BigInt = null
  def max:BigInt = null
  def range:Range = null
}

class DomainException(msg: String) extends Exception(msg)

class Range(val min: BigInt, val max: BigInt){
  import org.nrh.scream.Range._
  def intersection(that:Range):Option[Range] = {
    println("")
    println("this range = " + this)
    println("that range = " + that)
    var nmin = this.min max that.min
    var nmax = this.max min that.max
    println("result = " + new Range(nmin, nmax))

    if(nmin > nmax){
      return None
    }
    else{
      val nrange = new Range(nmin, nmax)
      if(nrange.min < this.min || nrange.max > this.max){
	throw new DomainException("intersection increased size of range")
      }
      return Some(nrange)
    }
  }

  def union(that:Range):Range = {
    range(this.min min that.min,
	  this.max max that.max)
  }

  override def toString = "Range(min = " + min + ", max = " + max + ")"

  override def equals(t1:Any):Boolean = {
    if(!t1.isInstanceOf[AnyRef]){
      println("range not instance of anyref")
      return false
    }
    else{
      val t2 = t1.asInstanceOf[AnyRef]
      if(this.getClass != t2.getClass){
	println("Range not instance of this.getClass")
	return false
      }
      else {
	val t3 = t2.asInstanceOf[Range]
	if(!((this.min == t3.min) && (this.max == t3.max))){
	  println("(this.min " + this.min + ") (t3.min " + t3.min + ")")
	  println("(this.max " + this.max + ") (t3.max " + t3.max + ")")
	  println("Range.min or Range.max != this.min and this.max")
	  return false
	}
	else {
	  return true
	}
      }
    }
  }

}

object Range {
  def range(x: BigInt) = new Range(x,x)
  def range(min: BigInt, max: BigInt) = new Range(min, max)
}

object Domain {
  def domain(range: Range): IntervalDomain = {
    return new IntervalDomain(range)
  }

  def domain(min: BigInt, max: BigInt): IntervalDomain = {
    return new IntervalDomain(new Range(min, max))
  }

  def domain(num: BigInt):IntervalDomain = {
    return new IntervalDomain(new Range(num,num))
  }
}
