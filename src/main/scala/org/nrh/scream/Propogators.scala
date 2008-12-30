package org.nrh.scream

trait Propogator {
  def propogate
}

class AdditionPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { 
    z := x.domain + y.domain
    x := z.domain - y.domain
    y := z.domain - x.domain
  }
}
class SubtractionPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { 
    z := x.domain - y.domain
    x := z.domain + y.domain
    y := x.domain - z.domain
  }
}
class MultiplicationPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { 
    z := x.domain * y.domain
    x := z.domain / y.domain
    y := z.domain / x.domain
  }
}
class DivisionPropogator(x:Var,y:Var,z:Var) extends Propogator {
  def propogate { 
    z := x.domain / y.domain
    x := z.domain * y.domain
    y := x.domain / z.domain
  }
}
class EqualityPropogator(x:Var,y:Var) extends Propogator {
  def propogate { 
    val intersection = x.domain intersect y.domain
    x := intersection
    y := intersection
  }
}
class NonPropogator extends Propogator {
  def propogate { null }
}
