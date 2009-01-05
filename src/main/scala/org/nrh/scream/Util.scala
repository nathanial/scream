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
}
