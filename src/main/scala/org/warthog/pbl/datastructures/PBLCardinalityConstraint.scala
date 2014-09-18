package org.warthog.pbl.datastructures

import scala.collection.mutable.ArrayBuffer

/**
 * Representation of a cardinality constraint
 * @param terms left-hand side of the constraint
 * @param degree right-hand side of the constraint
 */
class PBLCardinalityConstraint(var terms : List[PBLTerm], var degree : BigInt) extends Constraint(terms, degree){
  reduceCoefficients()

  var watchedLiterals = new ArrayBuffer[PBLTerm](degree.toInt + 1)

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
      ConstraintState.EMPTY
    else if(terms.size == degree) {
      //all literals have to be watched
      watchedLiterals = new ArrayBuffer[PBLTerm](terms.size)
      terms.copyToBuffer(watchedLiterals)
      //add clause to watchedList of all variables
      terms.map(_.l.v.add(this))
      ConstraintState.UNIT
    } else if(degree < 0)
      ConstraintState.SAT
    else {
      var i = 0
      //watch degree + 1 many terms
      terms.exists{t =>
        watchedLiterals(i) = t
        t.l.v.add(this)
        i += 1
        i > degree
     }
      ConstraintState.SUCCESS
    }

  }

  /**
   * Update the watched literals if necessary
   * @param v the assigned variable
   * @param value the assigned value
   * @return the new state of the constraint
   */
  override def updateWatchedLiterals(v: PBLVariable, value: Boolean)  = {
    var state = ConstraintState.SUCCESS
    for(i <- 0 to watchedLiterals.size-1; if watchedLiterals(i).l.v == v){
      val t = watchedLiterals(i)
      //literal evaluates to false => find new literal to watch
      if(t.l.phase != value){
        getNewWatchedLiteral match {
            //no new literal can be found => unit
            case None => {
              //constraint is unit
              if(this.isUnit(t)){
                state = ConstraintState.UNIT
               //constraint is empty
              } else {
                state = ConstraintState.EMPTY
              }
            }
             //new literal was found => success
            case Some(newTerm) => {
              //update the watched list of v (remove this constraint)
              v.remove(this)
              watchedLiterals.update(i,newTerm)
              newTerm.l.v.add(this)
            }
        }
      }
    }
    state
  }

  /**
   * Search for a new term with a literal which can be watched
   * @return None if no literal can be found else Some(PBLTerm)
   */
  private def getNewWatchedLiteral: Option[PBLTerm] = {
    var term: Option[PBLTerm] = None
    val diff = this.terms diff watchedLiterals
    if(diff.exists{t =>
      term = Some(t)
      t.l.v.state == State.OPEN || t.l.evaluates2True
    }) term
    else None
  }

  /**
   * Check if the constraint is unit or empty
   * @param t the term which can't be re-watched
   * @return true if the constraint is unit else false (and the constraint is empty)
   */
  private def isUnit(t: PBLTerm): Boolean = {
    watchedLiterals.-(t).foldLeft(true){(bol,t) =>
      bol && (t.l.v.state == State.OPEN || t.l.evaluates2True)}
  }
}
