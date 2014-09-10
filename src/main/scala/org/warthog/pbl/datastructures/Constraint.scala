package org.warthog.pbl.datastructures

/**
 *  Abstract base class for a pseudo-boolean constraints
 *  and cardinality constraints
 *  Form of a pseudo-boolean constraint: a_1x_1 + a_2x_2 + ... + a_nx_n >= degree
 *  Form of a cardinality constraint: x_1 + x_2 + x_3 + ... + x_n >= degree
 *  @param terms left-hand side of the constraint
 *  @param degree right-hand side of the constraint
 */
abstract class Constraint (val terms: List[PBLTerm], degree: BigInt){


}
