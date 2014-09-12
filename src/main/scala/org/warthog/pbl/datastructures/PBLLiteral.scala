package org.warthog.pbl.datastructures

import org.warthog.generic.formulas.Formula

/**
 * Representation of a pseudo-boolean literal
 * @param v a variable
 * @param phase the phase of the variable
 */
class PBLLiteral(val v: PBLVariable, val phase: Boolean = true){

  override def toString = if(phase) v.toString else Formula.NOT + v.toString

  def negate: PBLLiteral = new PBLLiteral(v, !phase)

  def copy = new PBLLiteral(v.copy, phase)
}

