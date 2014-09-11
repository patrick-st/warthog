package org.warthog.pbl.datastructures

/**
 * Representation of a pseudo-boolean term
 * @param a the coefficient
 * @param l a literal
 */
class PBLTerm(var a: BigInt, var l: PBLLiteral) {

  override def toString = a.toString() + " " + l.toString
}
