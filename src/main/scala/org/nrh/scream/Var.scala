package org.nrh.scream
import scala.collection.mutable.{Stack,ListBuffer}
import org.nrh.scream.Interval._
import org.nrh.scream.Domain._

class Var(val csp:CSP, val fromUser:Boolean) extends Logging {
  var name = "default"
  val domainStack = new Stack[Domain]
  private val constraintBuffer = new ListBuffer[Constraint]

  def domain = domainStack.top

  def constraints:List[Constraint] = constraintBuffer.toList

  def isFromUser:Boolean = fromUser

  def isSatisfied:Boolean = 
    this.isConsistent && constraints.forall(_.isSatisfied)

  def isConsistent:Boolean = domain != EmptyDomain 

  def isAssigned:Boolean = domain.isSingleton

  def pushDomain(d:Domain) {
    domainStack.push(d)
  }

  def constrain(constraint:Constraint){
    constraintBuffer += constraint
  }

  private def constrainAll(c:Constraint,vars:Var*){
    for(v <- vars){
      v constrain c
    }
  }
  
  override def toString:String = this.name

  def +(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    val add = new AdditionConstraint(x,y,z)
    constrainAll(add,x,y,z)
    return z
  }

  def -(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    val sub = new SubtractionConstraint(x,y,z)
    constrainAll(sub,x,y,z)
    return z
  }
   
  def /(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    val div = new DivisionConstraint(x,y,z)
    constrainAll(div,x,y,z)
    return z
  }

  def *(that:Var):Var = {
    val (x,y,z) = (this,that,csp.newAnonymousVar)
    val mult = new MultiplicationConstraint(x,y,z)
    constrainAll(mult,x,y,z)
    return z
  }

  def ==(that:Var):Var = {
    val (x,y) = (this,that)
    val eq = new EqualityConstraint(x,y)
    constrainAll(eq,x,y)
    return this
  }

  def /=(that:Var):Var = {
    val (x,y) = (this,that)
    val neq = new InEqualityConstraint(x,y)
    constrainAll(neq,x,y)
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
