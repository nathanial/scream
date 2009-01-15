package org.nrh.scream
import scala.collection.mutable.{Stack,ListBuffer}
import org.nrh.scream.Interval._
import org.nrh.scream.Domain._

class Var(val csp:CSP, val fromUser:Boolean) extends Logging {
  var name = "default"
  val domainStack = new Stack[Domain]

  def domain = domainStack.top

  def previousDomain = domainStack(domainStack.length - 2)

  def constraints:List[Constraint] = 
    csp.constraints.filter(_.containsVar(this)).toList

  def unsatisfiedConstraints:List[Constraint] = 
    constraints.filter(!_.isSatisfied)

  def isFromUser:Boolean = fromUser

  def isSatisfied:Boolean = 
    this.isConsistent && constraints.forall(_.isSatisfied)

  def isConsistent:Boolean = domain != EmptyDomain 

  def isAssigned:Boolean = domain.isSingleton

  def pushDomain(d:Domain) {
    domainStack.push(d)
  }

  override def toString:String = this.name

  def +(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    csp.addConstraint(new AdditionConstraint(x,y,z))
    return z
  }

  def -(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    csp.addConstraint(new SubtractionConstraint(x,y,z))
    return z
  }
   
  def /(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    csp.addConstraint(new DivisionConstraint(x,y,z))
    return z
  }

  def *(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    csp.addConstraint(new MultiplicationConstraint(x,y,z))
    return z
  }

  def ==(that:Var):Var = {
    val (x,y) = (this,that)
    csp.addConstraint(new EqualityConstraint(x,y))
    return this
  }

  def /=(that:Var):Var = {
    val (x,y) = (this,that)
    csp.addConstraint(new InEqualityConstraint(x,y))
    return this
  }

  def /=(that:Domain):Var = {
    val (x,y) = (this,csp.newAnonymousVar(that))
    return x./=(y)
  }

  def :=(that:Var):Var = this := that.domain
    
  def :=(that:Domain):Var = {
    logger.debug("{} := {}",this,that)
    val nd = this.domain intersect that
    if(this.domain != nd){
      logger.debug("setting {} to {}", this.name, nd)
      this assign nd
    }
    return this
  }

  def assign(that:Domain) {
    logger.debug("{} assign {}",this,that)
    domainStack.pop
    domainStack.push(that)
  }

}
