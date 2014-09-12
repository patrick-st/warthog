package org.warthog.pbl.datastructures

/**
 * Representation of a cardinality constraint
 * @param terms left-hand side of the constraint
 * @param degree right-hand side of the constraint
 */
class PBLCardinalityConstraint(var terms : List[PBLTerm], var degree : BigInt) extends Constraint(terms, degree){
  reduceCoefficients()

  /**
   * Reduce all coefficients to 1 and adapts the degree accordingly
   */
  def reduceCoefficients() = {
    val coeff = terms(0).a
    terms.map(_.a = BigInt(1))
    val tuple = (degree /% coeff)
    degree =  if (tuple._2 == 0) tuple._1 else tuple._1 + 1
  }

  def copy = new PBLCardinalityConstraint(terms.foldLeft(List[PBLTerm]())(_ :+ _.copy), degree)
}
