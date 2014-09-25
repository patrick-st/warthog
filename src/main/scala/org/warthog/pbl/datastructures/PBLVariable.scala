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
  var state: State.Value = State.OPEN
  var watched = ListBuffer[Constraint]()
  var level: Int = -1
  var reason: Constraint = null

  override def toString = name

  override def equals(p: Any) = {
    if(p.isInstanceOf[PBLVariable]){
      p.asInstanceOf[PBLVariable].name == this.name
    } else
      false
  }

  override def hashCode() = name.##

  def copy = this

  def add(c: Constraint) = watched += c

  def remove(c: Constraint) = watched -= c

  /**
   * Assign the variable and return an empty constraint if one exists else None
   * If an unit constraint was computed the unitConstraints-list is updated as side-effect
   * @param value the assigned value of the variable
   * @param unitConstraints a list of unit constraints which has to be updated
   * @param level the level of the variable
   * @param reason the reason why the variable is assigned
   * @return Some(empty-clause) or None
   */
  def assign(value: Boolean, unitConstraints: ListBuffer[Constraint], level: Int, reason: Constraint): Option[Constraint] = {
    var emptyConstraint: Option[Constraint] = None
    //update state, level and reason
    if(value)
      state = State.TRUE
    else
      state = State.FALSE
    this.level = level
    this.reason = reason

    //update the constraints in watched list
    watched.map{c =>
      c.updateWatchedLiterals(this,value) match {
        case ConstraintState.UNIT => unitConstraints += c
        case ConstraintState.EMPTY => emptyConstraint = Some(c)
        case _ =>
      }
    }
    emptyConstraint
  }
}

object State extends Enumeration {
  type State = Value
  val TRUE, FALSE, OPEN = Value
}
