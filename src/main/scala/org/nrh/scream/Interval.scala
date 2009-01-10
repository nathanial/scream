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
    if(this strictOverlap that){
      interval(this.min max that.min,
	       this.max min that.max)
    }
    else {
      EmptyInterval
    }
  }

  def remove(that:Interval):List[Interval] = {
    if(this strictOverlap that){
     val i = this intersect that
     if(this == i){
       return Nil
     }
     else if(i.min == min){
       val nmin = i.max
       return interval(nmin + 1, max) :: Nil
     } 
     else if(i.max == max) {
       val nmax = i.min
       return interval(min, nmax - 1) :: Nil
     }
     else {
       val i1 = interval(min, i.min - 1)
       val i2 = interval(i.max + 1, max)
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
      if(!hasNext) throw new RuntimeException("Interval doesn't have next!")
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

  def flatten(list:List[List[Interval]]):List[Interval] = {
      var acc:List[Interval] = Nil
      for(x <- list)
	acc = acc ++ x
      acc
  }

  def intersect(list: List[Interval]):List[Interval] = {
    val others = (x:Interval) => list.remove(_ eq x)
    list.flatMap(r => {
      val olap = others(r).filter(_ strictOverlap r)
      if(olap.isEmpty) Nil else union(olap.map(_ intersect r))
    }).filter(_ != Nil).removeDuplicates
  }

  def union(list: List[Interval]):List[Interval] = list match {
    case Nil => Nil
    case (r :: rs) => {
      val (olap, nolap) = rs.partition(_ overlap r)
      val nr = (r :: olap.toList).reduceLeft(_ union _)
      return nr :: union(nolap.toList)
    }
  }

  def remove(xs:List[Interval], ys:List[Interval]):List[Interval] = {
    val (olap,nolap) = xs.partition(x => ys.exists(_ strictOverlap x))
    val removed:List[Interval] = flatten(olap.map(o => {
      val overlapping = ys.filter(x => x strictOverlap o)
      overlapping.foldLeft(o :: Nil)((acc,x) => flatten(acc.map(y => y remove x)))
    }))
    return union(removed ++ nolap)
  }

  def areDisjoint(intervals:List[Interval]):Boolean = {
    val others = (x:Interval) => intervals.remove(_ eq x)
    for(i <- intervals)
      for(j <- others(i))
	if(i overlap j) 
	  return false
    return true
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
