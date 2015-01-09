package org.warthog.pbl.datastructures


/**
 * Representation of a pseudo-boolean constraint
 * @param terms terms of the left-hand side
 * @param degree the right-hand side
 */
class PBLConstraint(var terms: List[PBLTerm], var degree: BigInt) extends Constraint(terms, degree) {

  //sum of all coefficients a_i where l_i evaluates to true
  var currentSum: BigInt = 0
  //sum of all coefficients a_i where l_i not evaluates to false (l_i = true or open) minus degree
  var slack: BigInt = 0

  /**
   * Returns a copy of the constraint
   * @return the copied constraint
   */
  def copy = new PBLConstraint(terms.foldLeft(List[PBLTerm]())(_ :+ _.copy), degree)

  /**
   * Initialize the watched literals.
   * Note the counters technique is implemented for pseudo-boolean constraints,
   * which actually means all literals are watched
   * @return the state of the constraint:
   *         - UNIT, EMPTY, SAT
   *         - or UNRESOLVED if the constraint is currently unsat but can still be satisfied and
   *         the watched literals are successfully initialized
   */
  def initWatchedLiterals = {
    slack = terms.filter(! _.l.evaluates2False).map(_.a).sum - degree
    currentSum = terms.filter(_.l.evaluates2True).map(_.a).sum
    //add the clause to watched lists of all variables
    terms.map(_.l.v.add(this))
    getCurrentState
  }

  /**
   * Checks if the constraint is unit or not
   * @return true if constraint is unit else false
   */
  def isUnit: Boolean = {
    if (currentSum >= degree || slack < 0)
      false
    else
      terms.exists(_.a > slack)
  }

  /**
   * Update the two counters currentSum and slack
   * @param v the assigned variable
   * @param value the assigned value
   * @return the new state of the constraint
   */
  override def updateWatchedLiterals(v: PBLVariable, value: Boolean) = {
    val changedTerm = terms.find(_.l.v == v).get
      if (changedTerm.l.phase == value)
        //literal evaluates to true => currentSum has to be updated
        currentSum += changedTerm.a
       else
      //literal evaluates to false => slack has to be updated
        slack -= changedTerm.a
    //return the current state
    getCurrentState
  }

  /**
   * Computes all literals which has to be propagated
   * @return the literals to propagate
   */
  override def getLiteralsToPropagate = {
    if(this.isUnit) {
      terms.foldLeft(List[PBLLiteral]()) { (list, term) =>
        if (term.l.v.state == State.OPEN && term.a > slack)
          list :+ term.l
        else
          list
      }
    } else
      Nil
  }

  /**
   * Checks if the constraint is empty, unit or sat
   * @return the current state
   */
  def getCurrentState: ConstraintState.Value = {
    if (slack < 0)
      return ConstraintState.EMPTY
    if (currentSum >= degree)
      return ConstraintState.SAT
    if (this.isUnit)
      ConstraintState.UNIT
    else ConstraintState.UNRESOLVED
  }

  /**
   * Method updates the slack.
   * Necessary after unassigning a variable.
   */
  def updateSlack() = slack = getSlack

  /**
   * Method updates the current sum.
   * Necessary after unassigning a variable
   */
  def updateCurrentSum() = currentSum = terms.filter(_.l.evaluates2True).map(_.a).sum

}

