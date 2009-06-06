package org.nrh.scream
import scala.collection.mutable.{HashMap}

object Profiler {
  val profiles = new HashMap[Symbol, (BigInt, BigInt)]
  var active = false

  def reset {
    profiles.clear
    active = false
  }

  def time[A](msg:String)(fn: => A):A = {
    val start = System.currentTimeMillis
    val result = fn
    val end = System.currentTimeMillis
    println(msg + " took " + ((end - start) / 1000.0) + " seconds")
    return result
  }

  def timed[A](sym: Symbol)(fn: => A):A = {
    if(active){
      if(!profiles.keys.exists(_ == sym)){
	profiles.put(sym, (0, 0))
      }
      val start = System.currentTimeMillis
      val result = fn
      val end = System.currentTimeMillis
      val (oldTotal,count) = profiles(sym)
      val newTotal = oldTotal + end - start
      profiles.put(sym, (newTotal, count + 1))
      return result
    }
    else {
      return fn
    }
  }
    
}
