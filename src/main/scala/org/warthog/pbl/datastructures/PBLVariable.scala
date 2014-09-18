package org.warthog.pbl.datastructures


import scala.collection.mutable.ListBuffer

/**
 * Pseudo-boolean Variable
 * @param name the name of the variable.
 *             Note the variable name has to be of the form: x[Integer]
 */
class PBLVariable(val name: String) {
  //ID = Integer of the variable name
  val ID = name.drop(1).toInt
  var watched = ListBuffer[Constraint]()
  var level: Int = -1
  var state: State.Value = State.OPEN

  override def toString = name

  def copy = this

  def add(c: Constraint) = watched += c

  def remove(c: Constraint) = watched -= c
}

object State extends Enumeration {
  type State = Value
  val TRUE, FALSE, OPEN = Value
}
