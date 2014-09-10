package org.warthog.pbl.datastructures


/**
 * Representation of a pseudo-boolean constraint
 * @param terms terms of the left-hand side
 * @param degree the right-hand side
 */
class PBLConstraint(terms: List[PBLTerm], degree: BigInt) extends Constraint(terms, degree){

}
