package org.nrh.scream
import scala.collection.mutable.{ListBuffer}

object Util {
  val random = new scala.util.Random(System.currentTimeMillis)
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
    val ri:BigInt = Math.abs(random.nextInt)
    val i = (ri % d.length) + 1
    return d.min + i
  }

  def chooseRandomly[A](seq:Seq[A]):A = {
    return seq(random.nextInt(seq.length))
  }

  def randomize(l:List[BigInt]):List[BigInt] = {
    var ol = new ListBuffer[BigInt]
    ol ++= l
    val nl = new ListBuffer[BigInt]
    var count = 0
    while(count < ol.length){
      val n = chooseRandomly(l)
      ol -= n
      nl += n
      count += 1
    }

    return nl.toList
  }      

  def others[A](list:List[A])(x:A) = {
    val buff = new ListBuffer[A]
    buff ++= list
    buff -= x
    buff.toList
  }

  def pushNewDomains(vars:Seq[Var]){ 
    vars.foreach(v => v.domainStack.push(v.domain.shallowCopy)) 
  }

  def popDomains(vars:Seq[Var]) { 
    vars.foreach(v => v.domainStack.pop) 
  }

  def bind[A](vars:Seq[Var])(fn: => A):A = {
    pushNewDomains(vars)
    val result = fn
    popDomains(vars)
    return result
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


trait Publishing[A] {
  val subscribers = new ListBuffer[A => Unit]
  def addSubscriber(fn: A => Unit){
    subscribers += fn
  }

  def removeSubscriber(fn: A => Unit){
    subscribers -= fn
  }

  def publish(event:A){
    subscribers.toList.foreach(s => s(event))
  }
}
  
