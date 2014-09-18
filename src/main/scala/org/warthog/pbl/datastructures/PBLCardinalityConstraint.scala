package org.warthog.pbl.datastructures

import scala.collection.mutable.ArrayBuffer

/**
 * Representation of a cardinality constraint
 * @param terms left-hand side of the constraint
 * @param degree right-hand side of the constraint
 */
class PBLCardinalityConstraint(var terms : List[PBLTerm], var degree : BigInt) extends Constraint(terms, degree){
  reduceCoefficients()

  var watchedLiterals = new Array[PBLTerm](degree.toInt + 1)

  /**
   * Reduce all coefficients to 1 and adapt the degree accordingly
   */
  def reduceCoefficients() = {
    val coeff = terms(0).a
    terms.map(_.a = BigInt(1))
    val tuple = (degree /% coeff)
    degree =  if (tuple._2 == 0) tuple._1 else tuple._1 + 1
  }

  /**
   * Returns a copy of the constraint
   * @return the copied constraint
   */
  def copy = new PBLCardinalityConstraint(terms.foldLeft(List[PBLTerm]())(_ :+ _.copy), degree)

  def initWatchedLiterals() = {
    if(terms.size < degree)
      State.EMPTY
    else if(terms.size == degree) {
      //all literals have to be watched
      terms.copyToArray(watchedLiterals)
      //add clause to watchedList of all variables
      terms.map(_.l.v.add(this))
      State.UNIT
    } else if(degree < 0)
      State.SAT
    else {
      var i = 0
      //watch degree + 1 many terms
      terms.exists{t =>
        watchedLiterals(i) = t
        t.l.v.add(this)
        i += 1
        i > degree
     }
      State.SUCCESS
    }

  }



}
