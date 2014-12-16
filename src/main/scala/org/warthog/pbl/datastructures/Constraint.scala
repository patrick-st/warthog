package org.warthog.pbl.datastructures


/**
 * Abstract base class for a pseudo-boolean constraints
 * and cardinality constraints
 * Form of a pseudo-boolean constraint: a_1x_1 + a_2x_2 + ... + a_nx_n >= degree
 * Form of a cardinality constraint: x_1 + x_2 + x_3 + ... + x_n >= degree
 * Note that all constraints are normalized which means:
 * - all coefficients a_i > 0
 * - degree > 0
 * - relational operator: >=
 */
abstract class Constraint() {

  //left-hand side of the constraint
  var terms: List[PBLTerm]
  //right-hand side of the constraint
  var degree: BigInt
  //is it allowed to remove the constraint from the instance or not
  var removable: Boolean

  def this(terms: List[PBLTerm], degree: BigInt, removable: Boolean = false) {
    this()
    this.terms = terms
    this.degree = degree
    this.removable = removable
    normalize()
  }

  /**
   * String representation of the constraint
   * @return the string representation
   */
  override def toString = terms.map(_.toString() + " + ").foldLeft("")(_ + _).dropRight(2) + ">= " + degree

  override def equals(p: Any): Boolean = {
    if (p.isInstanceOf[Constraint]) {
      val otherTerms = p.asInstanceOf[Constraint].terms
      val otherDegree = p.asInstanceOf[Constraint].degree
      if (terms.size != otherTerms.size || degree != otherDegree)
        false
      else {
        for (t <- otherTerms)
          if (!terms.contains(t))
            return false
        true
      }
    } else
      false
  }

  override def hashCode() = terms.foldLeft(1)(_ & _.hashCode()) & degree.##

  /**
   * Normalize the constraint by eliminating all negative coefficients
   */
  def normalize() {
    terms.map { t =>
      if (t.a < 0) {
        t.a = t.a.abs
        t.l = t.l.negate
        degree += t.a
      }
    }
  }

  /**
   * Reduce the all coefficients which are greater than the degree
   */
  def reduce() {
    terms.map(t =>
     if(t.a > degree){
       t.a = degree
     }
    )
  }

  /**
   * Returns a copy of the constraint
   * @return the copied constraint
   */
  def copy: Constraint

  def *(x: BigInt) {
    terms.map(_.a *= x)
    degree *= x
  }

  /**
   * Initialize the watched literals.
   * @return the state of the constraint
   */
  def initWatchedLiterals: ConstraintState.Value

  /**
   * Update the watched literals and re-watch a new literal if necessary
   * @param v the assigned variable
   * @param value the assigned value
   * @return the new state of the constraint
   */
  def updateWatchedLiterals(v: PBLVariable, value: Boolean): ConstraintState.Value

  /**
   * Computes all literals which has to be propagated
   * @return the literals to propagate
   */
  def getLiteralsToPropagate: List[PBLLiteral]
}

object ConstraintState extends Enumeration {
  type ConstraintState = Value
  val UNIT, EMPTY, SAT, SUCCESS = Value
}
