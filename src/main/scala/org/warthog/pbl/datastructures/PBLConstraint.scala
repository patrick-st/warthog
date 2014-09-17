package org.warthog.pbl.datastructures


/**
 * Representation of a pseudo-boolean constraint
 * @param terms terms of the left-hand side
 * @param degree the right-hand side
 */
class PBLConstraint(var terms: List[PBLTerm], var degree: BigInt) extends Constraint(terms, degree){
  //sum of all coefficients a_i where l_i evaluates to true
  var currentSum: BigInt = 0
  //sum of all coefficients a_i where l_i not evaluates to true (l_i = false or open)
  var slack: BigInt = 0

  /**
   * Checks if the pseudo-boolean constraint is a cardinality constraint
   * @return true if the constraint is a cardinality constraint else false
   */
  def isCardinalityConstraint(): Boolean = {
    var set = Set[BigInt]()
    terms.foreach(set += _.a)
    set.size == 1
  }

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
   *         - or SUCCESS if the constraint is currently unsat but can still be satisfied and
   *           the watched literals are successfully initialized
   */
  def initWatchedLiterals() = {
    slack = terms.map(_.a).sum - degree
    //add the clause to watched lists of all variables
    terms.map(_.l.v.add(this))

    if(slack < 0)
      State.EMPTY
    else if(this.isUnit)
      State.UNIT
    else if(degree < 0)
      State.SAT
    else State.SUCCESS
  }

  /**
   * Checks if the constraint is unit or not
   * @return true if constraint is unit else false
   */
  def isUnit(): Boolean = {
    if(currentSum > degree)
      true
    else {
      for (t <- terms) {
        if (t.a > slack) {
          return true
        }
      }
      false
    }
  }
}

