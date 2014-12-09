package org.warthog.pbl.datastructures

/**
 * Representation of a pseudo-boolean term
 * @param a the coefficient
 * @param l a literal
 */
class PBLTerm(var a: BigInt, var l: PBLLiteral) {

  override def toString = a.toString() + " " + l.toString

  def copy = new PBLTerm(BigInt(a.toString()) ,l.copy)

  override def equals(p: Any) = {
    if(p.isInstanceOf[PBLTerm]){
      p.asInstanceOf[PBLTerm].a == a && p.asInstanceOf[PBLTerm].l == l
    } else
      false
  }

  override def hashCode() = l.hashCode() & a.##
}
