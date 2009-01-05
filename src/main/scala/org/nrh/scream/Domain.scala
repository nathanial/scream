package org.nrh.scream
import org.nrh.scream.Interval._
import org.nrh.scream.Util._
import org.nrh.scream.Domain._

trait Domain extends Iterable[BigInt] {
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

  def oldString:String = super.toString

  def length = {
    var count:BigInt = 0
    for(i <- intervals){
      count += i.length
    }
    count
  }
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
    
  override def toString:String = "EmptyDomain"

  private class EmptyIterator extends Iterator[BigInt] {
    def next:BigInt = unimplemented
    def hasNext = false
  }
}    

class DefaultDomain(val intervals: List[Interval]) extends Domain with Logging {
  verifyDisjointIntervals

  def intersect(that:Domain):Domain = {
    logger.debug("domain-intersect " + this + " " + that)
    val nintervals = intersectIntervals(this.intervals ++ that.intervals)
    logger.debug("domain-intersect result = " + nintervals)
    if(nintervals.length == 0)
      return EmptyDomain
    else
      return domain(nintervals)
  }

  private def flatten_intervals(list:List[List[Interval]]):List[Interval] = {
      var acc:List[Interval] = Nil
      for(x <- list)
	acc = acc ++ x
      acc
  }


  def remove(that:Domain):Domain = {
    if(this overlap that){
      val (olap,nolap) = intervals.partition(x => that.intervals.exists(_ strictOverlap x))
      val removed:List[Interval] = flatten_intervals(olap.map(o => {
	val overlapping = that.intervals.filter(x => x strictOverlap o)
	overlapping.foldLeft(o :: Nil)((acc,x) => flatten_intervals(acc.map(y => y remove x)))
      }))
      val nintervals = unionIntervals(removed ++ nolap)
      if(nintervals.length == 0)
	return EmptyDomain
      else
	return domain(nintervals)
    } else {
      return this
    }
  }

  def overlap(that:Domain):Boolean = {
    this.intervals.exists(x => that.intervals.exists(y => x strictOverlap y))
  }

  def union(that:Domain):Domain = 
    domain(unionIntervals(this.intervals ++ that.intervals))

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
    val nmin = if(that.max == 0) this.min else { this.min / that.max }
    val nmax = if(that.min == 0) this.max else { this.max / that.min }
    return domain(interval(nmin,nmax))
  }

  override def toString:String = {
    return "(Domain " + intervals.mkString(",") + ")"
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

  private def intersectIntervals(list: List[Interval]):List[Interval] = {
    val others = (x:Interval) => list.remove(_ eq x)
    list.flatMap(r => {
      val olap = others(r).filter(_ strictOverlap r)
      if(olap.isEmpty) Nil else unionIntervals(olap.map(_ intersect r))
    }).filter(_ != Nil).removeDuplicates
  }

  private def unionIntervals(list: List[Interval]):List[Interval] = list match {
    case Nil => Nil
    case (r :: rs) => {
      val (olap, nolap) = rs.partition(_ overlap r)
      val nr = (r :: olap.toList).reduceLeft(_ union _)
      return nr :: unionIntervals(nolap.toList)
    }
  }

  private def verifyDisjointIntervals {
    val others = (x:Interval) => intervals.remove(_ eq x)
    for(i <- intervals)
      for(j <- others(i))
	if(i overlap j) 
	  throw new IntervalException("Intervals overlap: "+ Array(i,j).mkString(" "))
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
