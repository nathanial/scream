package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Debug._

object Empty extends Domain(null) {
  override def intersect(that:Domain):Domain = Empty
  override def union(that:Domain):Domain = that
  override def min:BigInt = null
  override def max:BigInt = null
  override def toString:String = "Empty"
  override def +(that:Domain):Domain = {
    throw new IllegalArgumentException("Empty Domain no worky")
  }
  override def -(that:Domain):Domain = {
    throw new IllegalArgumentException("Emtpy Domain no worky")
  }
  override def *(that:Domain):Domain = {
    throw new IllegalArgumentException("Emtpy Domain no worky")
  }
  override def /(that:Domain):Domain = {
    throw new IllegalArgumentException("Emtpy Domain no worky")
  }
}    


class Domain(val range: Range) {
  import org.nrh.scream.Domain._

  def intersect(that:Domain):Domain = {   
    (this.range intersect that.range) match {
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

  def +(that:Domain):Domain = {
    debug("adding, %s + %s",this, that)
    val nmin = this.min + that.min
    val nmax = this.max + that.max
    val ndomain = domain(nmin, nmax)
    return ndomain
  }

  def -(that:Domain):Domain = {
    debug("subtracting, %s - %s",this, that)
    var nmin = this.min - that.max
    var nmax = this.max - that.min
    val ndomain = domain(nmin, nmax)
    return ndomain
  }

  def *(that:Domain):Domain = {
    debug("%s * %s",this,that)
    return domain(this.min * that.min, this.max * that.max)
  }

  def /(that:Domain):Domain = {
    //min = this.min / that.max
    //max = this.max / that.min
    debug("%s / %s",this,that)
    val nmin = if(that.max == 0) this.min else { this.min / that.max }
    val nmax = if(that.min == 0) this.max else { this.max / that.min }
    return domain(nmin, nmax)
  }


}

class DomainException(msg: String) extends Exception(msg)

class Range(val min: BigInt, val max: BigInt){
  import org.nrh.scream.Range._

  def intersect(that:Range):Option[Range] = {
    debug(this + " intersect " + that)
    var nmin = this.min max that.min
    var nmax = this.max min that.max
    debug("result = " + new Range(nmin, nmax))

    if(nmin > nmax){
      return None
    }
    else{
      val nrange = new Range(nmin, nmax)
      if(nrange.min < this.min || nrange.max > this.max){
	throw new DomainException("intersect increased size of range")
      }
      return Some(nrange)
    }
  }

  def union(that:Range):Range = {
    range(this.min min that.min,
	  this.max max that.max)
  }

  override def toString = "Range(min = " + min + ", max = " + max + ")"

}

object Range {
  def range(x: BigInt) = new Range(x,x)
  def range(min: BigInt, max: BigInt) = new Range(min, max)

  def same(r1:Range, r2:Range):Boolean = {
    r1.min == r2.min && r1.max == r2.max
  }

  def different(r1:Range, r2:Range):Boolean = !same(r1,r2)
}

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
  
  def same(d1:Domain, d2:Domain):Boolean = {
    d1.min == d2.min && d1.max == d2.max
  }

  def different(d1:Domain, d2:Domain):Boolean = !same(d1,d2)

  implicit def bigIntToDomain(num:BigInt):Domain = {
    return new Domain(new Range(num,num))
  }

  implicit def intToDomain(num:Int):Domain = {
    return new Domain(new Range(num,num))
  }
}
