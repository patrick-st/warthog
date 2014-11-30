package org.warthog.pbl.datastructures

import org.warthog.generic.formulas.Formula

/**
 * Representation of a pseudo-boolean literal
 * @param v a variable
 * @param phase the phase of the variable
 */
class PBLLiteral(var v: PBLVariable, val phase: Boolean = true){

  override def toString = if(phase) v.toString else Formula.NOT + v.toString

  def negate: PBLLiteral = new PBLLiteral(v, !phase)

  def copy = new PBLLiteral(v.copy, phase)

  override def equals(p: Any) = {
    if(p.isInstanceOf[PBLLiteral]){
      p.asInstanceOf[PBLLiteral].v == this.v && (p.asInstanceOf[PBLLiteral].phase == this.phase)
    } else
      false
  }

  override def hashCode() = v.hashCode() & phase.##

  def evaluates2True = (phase && v.state == State.TRUE) || (!phase && v.state == State.FALSE)

  def evaluates2False = (phase && v.state == State.FALSE) || (!phase && v.state == State.TRUE)
}

