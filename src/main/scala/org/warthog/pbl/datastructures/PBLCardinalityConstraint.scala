package org.warthog.pbl.datastructures

import org.warthog.pbl.datastructures.ConstraintState.ConstraintState

import scala.collection.mutable.ArrayBuffer

/**
 * Representation of a cardinality constraint
 * @param terms left-hand side of the constraint
 * @param degree right-hand side of the constraint
 */
class PBLCardinalityConstraint(var terms : List[PBLTerm], var degree : BigInt, var removable: Boolean = false) extends Constraint(terms, degree, removable){
  reduceCoefficients()

  var watchedLiterals = new ArrayBuffer[PBLTerm](degree.+(1).toInt)

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
  def copy = new PBLCardinalityConstraint(terms.foldLeft(List[PBLTerm]())(_ :+ _.copy), degree, this.removable)

  /**
   * Initialize the watched literals.
   * @return the state of the constraint
   */
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
    } else if(degree <= 0)
      ConstraintState.SAT
    else {
      var i = 0
      //watch degree + 1 many terms
      terms.exists{t =>
        watchedLiterals += t
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
  override def updateWatchedLiterals(v: PBLVariable, value: Boolean): ConstraintState = {
    for(t <- watchedLiterals; if t.l.v == v ){
      //if the corresponding literal evaluates to true, nothing has to be updated
      if(t.l.evaluates2True) return ConstraintState.SUCCESS
      //search for a new literal
      getNewWatchedLiteral match {
        //new literal was found
        case Some(newTerm) => {
          //remove the old literal and add the new literal
          watchedLiterals = watchedLiterals.filter(_ != t) += newTerm
          //update the watched lists of the variables
          t.l.v.remove(this)
          newTerm.l.v.add(this)
          return ConstraintState.SUCCESS
        }
        case None => {
          if(this.isUnit()) return ConstraintState.UNIT
          if(this.isSat()) return ConstraintState.SAT
        }
      }
      return ConstraintState.EMPTY
    }
    return ConstraintState.EMPTY
  }


  /**
   * Computes all literals which has to be propagated
   * @return the literals to propagate
   */
  override def getLiteralsToPropagate = {
    watchedLiterals.foldLeft(List[PBLLiteral]()){(list, term) =>
      if(term.l.v.state == State.OPEN)
        list :+ term.l
      else
        list
    }
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
   * Check if the constraint is unit or not
   * @return true if the constraint is unit else false
   */
  private def isUnit(): Boolean = {
    val openLiterals = watchedLiterals.count{_.l.v.state == State.OPEN}
    val trueLiterals = watchedLiterals.count{_.l.evaluates2True}
    openLiterals + trueLiterals == degree && trueLiterals < degree

  }

  /**
   * Checks if the constraint is satisfied or not
   * @return true if the constraint is satisfied else false
   */
  private def isSat(): Boolean = {
    watchedLiterals.count{_.l.evaluates2True} >= degree
  }
}

