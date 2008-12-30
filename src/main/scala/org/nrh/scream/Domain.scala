package org.nrh.scream
import org.nrh.scream.Range._
import org.nrh.scream.Util._
import org.nrh.scream.Domain._

trait Domain extends Iterable[BigInt] {
  def ranges:List[Range]
  def intersect(that:Domain):Domain 
  def union(that:Domain):Domain
  def contains(num:BigInt):Boolean
  def subset(that:Domain):Boolean
  def isSingleton:Boolean
  def min:BigInt
  def max:BigInt
  def +(that:Domain):Domain
  def -(that:Domain):Domain
  def *(that:Domain):Domain
  def /(that:Domain):Domain
}

object Empty extends Domain {
  def ranges:List[Range] = Nil
  def intersect(that:Domain):Domain = Empty
  def union(that:Domain):Domain = that
  def min:BigInt = null
  def max:BigInt = null
  def contains(num:BigInt) = false
  def subset(that:Domain) = true //Not sure if this is right
  def isSingleton = false
  def +(that:Domain):Domain = unimplemented
  def -(that:Domain):Domain = unimplemented
  def *(that:Domain):Domain = unimplemented
  def /(that:Domain):Domain = unimplemented
  def elements:Iterator[BigInt] = new EmptyIterator
  override def toString:String = "Empty"

  private class EmptyIterator extends Iterator[BigInt] {
    def next:BigInt = unimplemented
    def hasNext = false
  }
}    

class DefaultDomain(val ranges: List[Range]) extends Domain with Logging {
  verifyDisjointRanges

  def intersect(that:Domain):Domain = {
    logger.debug("domain-intersect " + this + " " + that)
    val result = domain(intersectRanges(this.ranges ++ that.ranges))
    logger.debug("domain-intersect result = " + result)
    return result
  }

  def union(that:Domain):Domain = 
    domain(unionRanges(this.ranges ++ that.ranges))

  def contains(num:BigInt):Boolean = ranges.exists(_.contains(num))
  
  def subset(that:Domain) = 
    that.contains(this.min) && that.contains(this.max)
    
  def min:BigInt = ranges.map(_.min).reduceLeft(_ min _)
 
  def max:BigInt = ranges.map(_.max).reduceLeft(_ max _)

  def isSingleton:Boolean = min == max
  
  def elements:Iterator[BigInt] = new DomainIterator

  def +(that:Domain):Domain = {
    logger.debug("adding, %s + %s",this, that)
    val nmin = this.min + that.min
    val nmax = this.max + that.max
    val ndomain = domain(nmin upto nmax)
    return ndomain
  }

  def -(that:Domain):Domain = {
    logger.debug("subtracting, %s - %s",this, that)
    val nmin = this.min - that.max
    val nmax = this.max - that.min
    val ndomain = domain(nmin upto nmax)
    return ndomain
  }

  def *(that:Domain):Domain = {
    logger.debug("%s * %s",this,that)
    val nmin = this.min * that.min
    val nmax = this.max * that.max
    return domain(nmin upto nmax)
  }

  def /(that:Domain):Domain = {
    //min = this.min / that.max
    //max = this.max / that.min
    logger.debug("%s / %s",this,that)
    val nmin = if(that.max == 0) this.min else { this.min / that.max }
    val nmax = if(that.min == 0) this.max else { this.max / that.min }
    return domain(nmin upto nmax)
  }

  override def toString:String = {
    return "(Domain " + ranges.mkString(",") + ")"
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
      (this.ranges.length == _that.ranges.length) &&    
      zipSame(this.ranges.toList, _that.ranges.toList)
    }
  }

  private def intersectRanges(list: List[Range]):List[Range] = list match  {
    case Nil => Nil
    case (r :: rs) => {
      logger.debug("r = " + r)
      logger.debug("rs = " + rs)
      val (olap, nolap) = rs.partition(_ strictOverlap r)
      logger.debug("olap = " + olap)
      logger.debug("nolap = " + nolap)

      if(!olap.isEmpty){
	return (unionRanges(olap.map(_ intersect r)) ++ 
		intersectRanges(nolap.toList))
      }
      else if(olap.isEmpty && rs != Nil){	
	return intersectRanges(nolap.toList)
      }
      else {
	return r :: Nil
      }
    }
  }

  private def unionRanges(list: List[Range]):List[Range] = list match {
    case Nil => Nil
    case (r :: rs) => {
      val (olap, nolap) = rs.partition(_ overlap r)
      val nr = (r :: olap.toList).reduceLeft(_ union _)
      return nr :: unionRanges(nolap.toList)
    }
  }

  private def verifyDisjointRanges {
    var i = 0
    while(i < ranges.length){
      val ri = ranges(i)
      var j = 0
      while(j < ranges.length){
	if(j != i){
	  val rj = ranges(j)
	  if(ri overlap rj){
	    throw new RangeException("Ranges " + ri + " " + rj + 
				     " overlap in domain")
	  }
	}
	j += 1
      }
      i += 1
    }
  }

  private def zipSame(l1: List[Range], l2: List[Range]):Boolean = {
    if(l1.isEmpty && l2.isEmpty){
      return true
    }
    else{
      val r = l1.first
      val same = (x:Range) => r == x
      return l2.exists(same) && zipSame(l1.drop(1), l2.remove(same))
    }
  }

  private class DomainIterator extends Iterator[BigInt] {
    var cursor = 0
    var iter:Iterator[BigInt] = null
    if(!ranges.isEmpty){
      iter = ranges(cursor).elements
    }
    
    def hasNext:Boolean = {
      if(!ranges.isEmpty){
	if(iter.hasNext){
	  return true
	}
	else{
	  cursor += 1
	  if(cursor < ranges.length){
	    iter = ranges(cursor).elements
	    return iter.hasNext
	  }
	  else{
	    return false
	  }
	}
      }
      else {
	return false
      }
    }

    def next = iter.next
  }
}

object Domain {
  def domain(range: Range): Domain = {
    return new DefaultDomain(range :: Nil)
  }

  def domain(ranges: List[Range]):Domain = {
    return new DefaultDomain(ranges)
  }

  def domain(ranges: Range*):Domain = {
    return new DefaultDomain(ranges.toList)
  }

  implicit def bigIntToDomain(num:BigInt):Domain = {
    return domain(range(num,num))
  }

  implicit def intToDomain(num:Int):Domain = {
    return domain(range(num,num))
  }
}

class DomainException(msg: String) extends Exception(msg)
