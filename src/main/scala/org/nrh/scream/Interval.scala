package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Util._
import org.nrh.scream.Interval._

trait Interval extends Iterable[BigInt] {
  def min:BigInt
  def max:BigInt
  def intersect(that:Interval):Interval
  def union(that:Interval):Interval
  def remove(that:Interval):List[Interval]
  def overlap(that:Interval):Boolean 
  def strictOverlap(that:Interval):Boolean
  def length:BigInt
  def contains(num:BigInt):Boolean
}

class DefaultInterval(val min: BigInt, val max: BigInt) extends Interval with Logging {
  verifyConsistency

  def intersect(that:Interval):Interval = {
    logger.debug(this + " intersect " + that)
    val nmin = this.min max that.min
    val nmax = this.max min that.max
    logger.debug("result = Interval(" + nmin + "," + nmax + ")")
    return if(nmin > nmax) EmptyInterval else interval(nmin,nmax)
  }

  def remove(that:Interval):List[Interval] = {
    if(this overlap that){
     val intersection = this intersect that
     if(intersection == EmptyInterval) return this :: Nil
     val imin = intersection.min
     val imax = intersection.max
     if(imin == min && imax == max){
       return Nil
     }
     else if(imin == min){
       val nmin = imax
       return interval(nmin + 1, max) :: Nil
     } 
     else if(imax == max) {
       val nmax = imin
       return interval(min, nmax - 1) :: Nil
     }
     else {
       val i1 = interval(min, imin - 1)
       val i2 = interval(imax + 1, max)
       return i1 :: i2 :: Nil
     }
    }
    else {
      return this :: Nil
    }
  }

  def elements:Iterator[BigInt] = new IntervalIterator

  def union(that:Interval):Interval = {
    verifyOverlap(this,that)
    return interval(this.min min that.min,
		 this.max max that.max)
  }

  def overlap(that:Interval):Boolean = {
    val near = (x:BigInt, y:Interval) => {
      y.contains(x) ||
      y.contains(x + 1) ||
      y.contains(x - 1)
    }
    return (near(this.max,that) || near(this.min,that) ||
	    near(that.max,this) || near(that.min,this))
  }

  def strictOverlap(that:Interval):Boolean = {
    (that.contains(this.max) || that.contains(this.min) ||
     this.contains(that.max) || this.contains(that.min))
  }

  def length:BigInt = (this.max - this.min) + 1

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
      val _that = that.asInstanceOf[Interval]
      (this.min == _that.min) && (this.max == _that.max)
    }
  }

  private def verifyConsistency { 
    if(min > max){
      throw new IntervalException("min greater than max: " + this)
    }
  }

  private def verifyOverlap(r1:Interval, r2:Interval) {
    if(!(r1 overlap r2))
      throw new IntervalException("Unifying two intervals that do not overlap: " + 
			       r1 + " " + r2)
  }

  private class IntervalIterator extends Iterator[BigInt] {
    var cursor:BigInt = 0
    def hasNext:Boolean = (cursor + min) <= max
    def next:BigInt = {
      val ret = cursor + min
      cursor += 1
      return ret
    }
  }
}

      

object EmptyInterval extends Interval{
  def intersect(that:Interval):Interval = EmptyInterval
  def union(that:Interval):Interval = that
  def remove(that:Interval):List[Interval] = Nil
  def length = 0
  def contains(x:BigInt) = false
  def overlap(that:Interval) = false
  def strictOverlap(that:Interval) = false
  def max = unimplemented
  def min = unimplemented
  def elements = unimplemented
  override def toString = "EmptyInterval"
}


object Interval {
  def interval(x: BigInt) = new DefaultInterval(x,x)

  def interval(min: BigInt, max: BigInt) = new DefaultInterval(min, max)

  def maximal(l:Seq[Interval]):Interval = {
    l.reduceLeft{
      (x,y) => {
	if(x.length > y.length) x else y
      }
    }
  }

}

object IntervalImplicits {
  implicit def intToTempInterval(x: Int) = new Object {
    def upto(y:Int):Interval = {
      new DefaultInterval(x,y)
    }
  }

  implicit def bigIntToTempInterval(x:BigInt) = new Object {
    def upto(y:BigInt):Interval = {
      new DefaultInterval(x,y)
    }
  }

  implicit def intToInterval(x:Int) = new DefaultInterval(x,x)
}  


class IntervalException(msg: String) extends Exception(msg)
