package org.nrh.scream

object Util {
  def unimplemented = {
    throw new UnsupportedOperationException()
  }

  def fixedPoint(fn: => Boolean){
    var continue = true
    while(continue){
      continue = fn
    }
  }

  def changes[A,B](f: => A)(g: => B):Boolean = {
    val start = f
    g
    val finish = f
    return (start != finish)
  }
}
