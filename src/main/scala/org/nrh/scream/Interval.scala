package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Util._
import org.nrh.scream.Interval._

trait Interval extends Iterable[BigInt] with Ordered[Interval] {
  verify(min <= max, "min cannot be greater than max")

  def min:BigInt
  def max:BigInt
  def bounds:(BigInt,BigInt) = (min,max)
  def intersect(that:Interval):Interval
  def union(that:Interval):Interval
  def remove(that:Interval):List[Interval]
  def overlap(that:Interval):Boolean 
  def strictOverlap(that:Interval):Boolean
  def length:BigInt
  def contains(num:BigInt):Boolean

  def compare(that:Interval)= {
    if(this.length > that.length) 1
    else if(this.length == that.length) 0
    else -1
  }
}

class DefaultInterval(val min: BigInt, val max: BigInt) extends Interval with Logging {
  lazy val length:BigInt = (this.max - this.min) + 1
  override lazy val toString = "(" + min + " upto " + max + ")"

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
    verify(this overlap that, "intervals must overlap to unify")
    return interval(this.min min that.min,
		    this.max max that.max)
  }

  def overlap(that:Interval):Boolean = {
    def near(x:BigInt, y:Interval) = {
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

  def contains(num:BigInt):Boolean = 
    (this.min <= num) && (num <= this.max)

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

  private class IntervalIterator extends Iterator[BigInt] {
    var cursor:BigInt = 0
    def hasNext:Boolean = (cursor + min) <= max
    def next:BigInt = {
      verify(hasNext, "Interval doesn't have a next!")
      val ret = cursor + min
      cursor += 1
      return ret
    }
  }
}

      

object EmptyInterval extends Interval{
  val length:BigInt = 0
  lazy val max = unimplemented
  lazy val min = unimplemented
  lazy val elements = unimplemented
  override lazy val toString = "EmptyInterval"

  def intersect(that:Interval):Interval = EmptyInterval
  def union(that:Interval):Interval = that
  def remove(that:Interval):List[Interval] = Nil
  def contains(x:BigInt) = false
  def overlap(that:Interval) = false
  def strictOverlap(that:Interval) = false
}


object Interval {
  def interval(x: BigInt) = new DefaultInterval(x,x)

  def interval(min: BigInt, max: BigInt) = new DefaultInterval(min, max)

  def maximal(l:Seq[Interval]):Interval = {
    l.reduceLeft((x,y) => if(x > y) x else y)
  }

  def flatten(list:List[List[Interval]]):List[Interval] = {
    val acc:List[Interval] = Nil
    list.foldLeft(acc)(_ ++ _)
  }

  def intersect(list: List[Interval]):List[Interval] = {
    val others = Util.others(list)_
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
    xs.flatMap(x => {
      val olap = ys.filter(_ strictOverlap x)
      olap.foldLeft(x :: Nil)((acc,o) => flatten(acc.map(_ remove o)))
    })
  }

  def areDisjoint(intervals:List[Interval]):Boolean = {
    val others = Util.others(intervals)_
    for(i <- intervals; j <- others(i))
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
