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

  def verify(test: => Boolean, msg: => String){
    if(!test){
      throw new VerificationException(msg)
    }
  }

  def chooseRandomly(d:Domain):BigInt = {
    val random = new scala.util.Random
    val ri:BigInt = Math.abs(random.nextInt)
    val i = (ri % d.length) + 1
    return d.min + i
  }

}

class VerificationException(msg:String) extends RuntimeException(msg)

class SuperRichBoolean(b:Boolean) {
  def orElse(fn: => Boolean):Boolean = {
    b || fn
  }
}

object UtilImplicits {
  implicit def boolToSuperRich(b:Boolean):SuperRichBoolean = {
    new SuperRichBoolean(b)
  }
}
  
  
  
