package org.nrh.scream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.nrh.scream.Domain._

class DomainArithmetic(_this: Domain){
  def +(that:Domain):Domain = {
    guardEmpty(_this, that){
      val nmin = _this.min + that.min
      val nmax = _this.max + that.max
      val ndomain = domain(nmin, nmax)
      return ndomain
    }
  }

  def -(that:Domain):Domain = {
    guardEmpty(_this, that){
      var nmin = _this.min - that.max
      var nmax = _this.max - that.min
      val ndomain = domain(nmin, nmax)
      return ndomain
    }
  }

  def *(that:Domain):Domain = {
    guardEmpty(_this, that) {
      return domain(_this.min * that.min, _this.max * that.max)
    }
  }

  def /(that:Domain):Domain = {
    //min = _this.min / that.max
    //max = _this.max / that.min

    guardEmpty(_this, that) {
      val nmin = if(that.max == 0) _this.min else { _this.min / that.max }
      val nmax = if(that.min == 0) _this.max else { _this.max / that.min }
      return domain(nmin, nmax)
    }
  }


  private def guardEmpty[A](domains: Domain*)(fn: => A):A = {
    if(domains.exists(_ == Empty)){
      throw new DomainException("empty domains")
    }
    else{
      return fn
    }
  }

}

object DomainArithmetic {
  implicit def domain_to_domainArithmetic(domain: Domain):DomainArithmetic = {
    return new DomainArithmetic(domain)
  }
}
