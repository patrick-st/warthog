package org.warthog.pbl.datastructures


/**
 * Representation of a pseudo-boolean constraint
 * @param terms terms of the left-hand side
 * @param degree the right-hand side
 */
class PBLConstraint(var terms: List[PBLTerm], var degree: BigInt) extends Constraint(terms, degree){

  /**
   * Checks if the pseudo-boolean constraint is a cardinality constraint
   * @return true if the constraint is a cardinality constraint else false
   */
  def isCardinalityConstraint(): Boolean = {
    var set = Set[BigInt]()
    terms.foreach(set += _.a)
    set.size == 1
  }
}

