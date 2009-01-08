package org.nrh.scream
import scala.collection.mutable.ListBuffer

class Matrix[A](val matrix:List[A]) {
  
  def row(x:Int):List[A] = {
    matrix.drop(x * 9).take(9)
  }

  def rows:List[List[A]] = {
    (0 to 8).map(x => row(x)).toList
  }

  def column(y:Int):List[A] = {
    val ls = new ListBuffer[A]
    var cursor = 0
    while(cursor < 9){
      ls += row(cursor)(y)
      cursor += 1
    }
    ls.toList
  }

  def columns:List[List[A]] = {
    (0 to 8).map(x => column(x)).toList
  }

  def chunk(x:Int,y:Int):List[A] = {
    val r = row(y)
    return r(x) :: r(x+1) :: r(x+2) :: Nil
  }

  def square(x:Int,y:Int):List[A] = {
    chunk(x,y) ++ 
    chunk(x,y+1) ++
    chunk(x,y+2)
  }

  def squares:List[List[A]] = {
    square(0,0) :: square(3,0) :: square(6,0) ::
    square(0,3) :: square(3,3) :: square(6,3) ::
    square(0,6) :: square(3,6) :: square(6,6) :: Nil
  }


}

    
