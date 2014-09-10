package org.warthog.pbl.datastructures

/**
 * Representation of a cardinality constraint
 * @param terms left-hand side of the constraint
 * @param degree right-hand side of the constraint
 */
class PBLCardinalityConstraint(terms: List[PBLTerm], degree: BigInt) extends Constraint(terms, degree){

}
