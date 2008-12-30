package org.nrh.scream

trait Propogator {
  def propogate
}

class AdditionPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { null }
}
class SubtractionPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { null }
}
class MultiplicationPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { null }
}
class DivisionPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { null }
}
class EqualityPropogator(x:Var,y:Var) extends Propogator {
  def propogate { null }
}
class NonPropogator extends Propogator {
  def propogate { null }
}
