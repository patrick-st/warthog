package org.warthog.pbl.datastructures

import org.warthog.generic.formulas.Variable
import org.warthog.pbl.PBL

import scalaz.Id

/**
 * Pseudo-boolean Variable
 * @param name the name of the variable.
 *             Note the variable name has to be of the form: x[Integer]
 */
class PBLVariable(val name: String) {
  //ID = Integer of the variable name
  val ID = name.drop(1).toInt
  var watched = List[PBLConstraint]()
  var level: Int = -1

  override def toString = name

  def copy = this

  def add(other: Constraint) = watched +:= other


}
