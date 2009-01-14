package org.nrh.scream
import org.nrh.scream.Interval._
import org.nrh.scream.Util._
import org.nrh.scream.Domain._

trait Domain extends Iterable[BigInt] with Ordered[Domain] {
  def intervals:List[Interval]
  def intersect(that:Domain):Domain 
  def union(that:Domain):Domain
  def remove(that:Domain):Domain
  def contains(num:BigInt):Boolean
  def subset(that:Domain):Boolean
  def overlap(that:Domain):Boolean
  def isSingleton:Boolean
  def min:BigInt
  def max:BigInt
  def +(that:Domain):Domain
  def -(that:Domain):Domain
  def *(that:Domain):Domain
  def /(that:Domain):Domain
  def toBigInt:BigInt

  def oldString:String = super.toString

  def length = {
    var count:BigInt = 0
    for(i <- intervals){
      count += i.length
    }
    count
  }

  def compare(that:Domain) = {
    if(this.length > that.length) 1
    else if(this.length == that.length) 0
    else -1
  }

  def shallowCopy = this

}

object EmptyDomain extends Domain {
  def intervals:List[Interval] = Nil
  def intersect(that:Domain):Domain = EmptyDomain
  def union(that:Domain):Domain = that
  def remove(that:Domain):Domain = EmptyDomain
  def min:BigInt = 0
  def max:BigInt = 0
  def contains(num:BigInt) = false
  def subset(that:Domain) = true //Not sure if this is right
  def overlap(that:Domain) = false
  def isSingleton = false
  def +(that:Domain):Domain = EmptyDomain
  def -(that:Domain):Domain = EmptyDomain
  def *(that:Domain):Domain = EmptyDomain
  def /(that:Domain):Domain = EmptyDomain
  def elements:Iterator[BigInt] = new EmptyIterator
  def toBigInt:BigInt = null
    
  override def toString:String = "EmptyDomain"

  private class EmptyIterator extends Iterator[BigInt] {
    def next:BigInt = unimplemented
    def hasNext = false
  }
}    

class DefaultDomain(val intervals: List[Interval]) extends Domain with Logging {
  if(!Interval.areDisjoint(intervals)){
    throw new DomainException("Intervals for domain must be disjoint")
  }

  def intersect(that:Domain):Domain = {
    Interval.intersect(this.intervals ++ that.intervals) match {
      case Nil => EmptyDomain
      case xs => domain(xs)
    }
  }

  def remove(that:Domain):Domain = {
    if(!(this overlap that)) this
    else {
      Interval.remove(this.intervals,that.intervals) match {
	case Nil => EmptyDomain
	case xs => domain(xs)
      }
    }
  }

  def overlap(that:Domain):Boolean = {
    this.intervals.exists(x => that.intervals.exists(y => x strictOverlap y))
  }

  def union(that:Domain):Domain = 
    domain(Interval.union(this.intervals ++ that.intervals))

  def contains(num:BigInt):Boolean = intervals.exists(_.contains(num))
  
  def subset(that:Domain) = 
    that.contains(this.min) && that.contains(this.max)
    
  def min:BigInt = intervals.map(_.min).reduceLeft(_ min _)
 
  def max:BigInt = intervals.map(_.max).reduceLeft(_ max _)

  def isSingleton:Boolean = min == max
  
  def elements:Iterator[BigInt] = new DomainIterator

  def +(that:Domain):Domain = {
    logger.debug("adding, {} + {}",this, that)
    return domain(interval(this.min + that.min,
			   this.max + that.max))
  }

  def -(that:Domain):Domain = {
    logger.debug("subtracting, {} - {}",this, that)
    return domain(interval(this.min - that.max,
			   this.max - that.min))
  }

  def *(that:Domain):Domain = {
    logger.debug("{} * {}",this,that)
    return domain(interval(this.min * that.min,
			   this.max * that.max))
  }

  def /(that:Domain):Domain = {
    //min = this.min / that.max
    //max = this.max / that.min
    logger.debug("{} / {}",this,that)
    if(that == singleton(0)){
      throw new IllegalArgumentException("Division by zero is not permitted!!")
    }
    val nmin = if(that.max == 0) this.min else { this.min / that.max }
    val nmax = if(that.min == 0) this.max else { this.max / that.min }
    return domain(interval(nmin,nmax))
  }

  def toBigInt:BigInt = {
    if(!isSingleton) 
      throw new DomainException(
	"In order to convert to BigInt, a domain must be a singleton")
    else 
      this.elements.next
  }    

  override def toString:String = {
    if(isSingleton){
      return min.toString
    }
    else{
      return "(Domain " + intervals.mkString(",") + ")"
    }
  }

  override def equals(that:Any) = {
    if(that == null)
      false
    else if(!that.isInstanceOf[AnyRef])
      false
    else if(that.asInstanceOf[AnyRef].getClass != this.getClass)
      false
    else{
      val _that = that.asInstanceOf[DefaultDomain]
      (this.intervals.length == _that.intervals.length) &&    
      zipSame(this.intervals.toList, _that.intervals.toList)
    }
  }

  private def zipSame(l1: List[Interval], l2: List[Interval]):Boolean = {
    if(l1.isEmpty && l2.isEmpty){
      return true
    }
    else{
      val r = l1.first
      val same = (x:Interval) => r == x
      return l2.exists(same) && zipSame(l1.drop(1), l2.remove(same))
    }
  }

  private class DomainIterator extends Iterator[BigInt] {
    var cursor = 0
    var iter:Iterator[BigInt] = null
    if(!intervals.isEmpty){
      iter = intervals(cursor).elements
    }
    
    def hasNext:Boolean = {
      if(intervals.isEmpty) return false
      if(iter.hasNext){
	return true
      }
      else{
	cursor += 1
	if(cursor < intervals.length){
	  iter = intervals(cursor).elements
	  return iter.hasNext
	}
	else{
	  return false
	}
      }
    }

    def next = iter.next
  }
}

object Domain {
  def singleton(num:Int) = domain(interval(num,num))
  def singleton(num:BigInt) = domain(interval(num,num))

  def domain(range: Interval): Domain = {
    return new DefaultDomain(range :: Nil)
  }

  def domain(intervals: List[Interval]):Domain = {
    return new DefaultDomain(intervals)
  }

  def domain(intervals: Interval*):Domain = {
    return new DefaultDomain(intervals.toList)
  }

  def default_domain:Domain = {
    new DefaultDomain(interval(0,10000000) :: Nil)
  }

}

object DomainImplicits {
  implicit def bigIntToDomain(num:BigInt):Domain = {
    return domain(interval(num,num))
  }

  implicit def intToDomain(num:Int):Domain = {
    return domain(interval(num,num))
  }
}

class DomainException(msg: String) extends Exception(msg)
