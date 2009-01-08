package org.nrh.scream
import scala.collection.mutable.ListBuffer

class Matrix[A](val els:Seq[A]) extends Seq[A] {
  
  def row(x:Int):Seq[A] = {
    els.drop(x * 9).take(9)
  }

  def rows:Seq[Seq[A]] = {
    (0 to 8).map(x => row(x)).toSeq
  }

  def column(y:Int):Seq[A] = {
    val ls = new ListBuffer[A]
    var cursor = 0
    while(cursor < 9){
      ls += row(cursor)(y)
      cursor += 1
    }
    ls.toSeq
  }

  def columns:Seq[Seq[A]] = {
    (0 to 8).map(x => column(x)).toSeq
  }

  def chunk(x:Int,y:Int):Seq[A] = {
    val r = row(y)
    return r(x) :: r(x+1) :: r(x+2) :: Nil
  }

  def square(x:Int,y:Int):Seq[A] = {
    chunk(x,y) ++ 
    chunk(x,y+1) ++
    chunk(x,y+2)
  }

  def squares:Seq[Seq[A]] = {
    square(0,0) :: square(3,0) :: square(6,0) ::
    square(0,3) :: square(3,3) :: square(6,3) ::
    square(0,6) :: square(3,6) :: square(6,6) :: Nil
  }

  override def toString:String = {
    val buf = new StringBuilder()
    buf.append("\n")
    for(r <- rows){
      for(m <- r){
	buf.append(m.toString)
	buf.append(" ")
      }
      buf.append("\n")
    }
    return buf.toString
  }

  def length = els.length

  def elements = els.elements

  def apply(idx:Int):A = els(idx)

  def sameAs[B](that:Matrix[B]):Boolean = {
    if(this.length == that.length){
      val i1 = this.elements
      val i2 = that.elements
      while(i1.hasNext){
	val v1 = i1.next
	val v2 = i2.next
	if(v1 != v2) return false
      }
      return true
    }
    else {
      return false
    }
  }
}

    
