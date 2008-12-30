package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Util._
import org.nrh.scream.Range._

trait Range extends Iterable[BigInt] {
  def min:BigInt
  def max:BigInt
  def intersect(that:Range):Range
  def union(that:Range):Range
  def overlap(that:Range):Boolean 
  def strictOverlap(that:Range):Boolean
  def length:BigInt
  def contains(num:BigInt):Boolean
}

class DefaultRange(val min: BigInt, val max: BigInt) extends Range with Logging {
  verifyConsistency

  def intersect(that:Range):Range = {
    logger.debug(this + " intersect " + that)
    var nmin = this.min max that.min
    var nmax = this.max min that.max
    logger.debug("result = Range(" + nmin + "," + nmax + ")")
    return if(nmin > nmax) EmptyRange else range(nmin,nmax)
  }

  def elements:Iterator[BigInt] = new RangeIterator

  def union(that:Range):Range = {
    verifyOverlap(this,that)
    return range(this.min min that.min,
		 this.max max that.max)
  }

  def overlap(that:Range):Boolean = {
    val near = (x:BigInt, y:Range) => {
      y.contains(x) ||
      y.contains(x + 1) ||
      y.contains(x - 1)
    }
    return (near(this.max,that) || near(this.min,that) ||
	    near(that.max,this) || near(that.min,this))
  }

  def strictOverlap(that:Range):Boolean = {
    (that.contains(this.max) || that.contains(this.min) ||
     this.contains(that.max) || this.contains(that.min))
  }

  def length:BigInt = this.max - this.min    

  def contains(num:BigInt):Boolean = 
    (this.min <= num) && (num <= this.max)

  override def toString = "(" + min + " upto " + max + ")"

  override def equals(that:Any):Boolean = {
    if(that == null)
      false
    else if(!that.isInstanceOf[AnyRef])
      false
    else if(that.asInstanceOf[AnyRef].getClass != this.getClass)
      false
    else {
      val _that = that.asInstanceOf[Range]
      (this.min == _that.min) && (this.max == _that.max)
    }
  }

  private def verifyConsistency { 
    if(min > max){
      throw new RangeException("min greater than max: " + this)
    }
  }

  private def verifyOverlap(r1:Range, r2:Range) {
    if(!(r1 overlap r2))
      throw new RangeException("Unifying two ranges that do not overlap: " + 
			       r1 + " " + r2)
  }

  private class RangeIterator extends Iterator[BigInt] {
    var cursor:BigInt = 0
    def hasNext:Boolean = (cursor + min) <= max
    def next:BigInt = {
      val ret = cursor + min
      cursor += 1
      return ret
    }
  }
}

      

object EmptyRange extends Range{
  def intersect(that:Range):Range = EmptyRange
  def union(that:Range):Range = that
  def length = 0
  def contains(x:BigInt) = false
  def overlap(that:Range) = false
  def strictOverlap(that:Range) = false
  def max = unimplemented
  def min = unimplemented
  def elements = unimplemented
}


object Range {
  def range(x: BigInt) = new DefaultRange(x,x)

  def range(min: BigInt, max: BigInt) = new DefaultRange(min, max)

  def maximal(l:Seq[Range]):Range = {
    l.reduceLeft{
      (x,y) => {
	if(x.length > y.length) x else y
      }
    }
  }

  implicit def intToTempRange(x: Int) = new Object {
    def upto(y:Int):Range = {
      new DefaultRange(x,y)
    }
  }

  implicit def bigIntToTempRange(x:BigInt) = new Object {
    def upto(y:BigInt):Range = {
      new DefaultRange(x,y)
    }
  }

  implicit def intToRange(x:Int) = new DefaultRange(x,x)

}


class RangeException(msg: String) extends Exception(msg)
