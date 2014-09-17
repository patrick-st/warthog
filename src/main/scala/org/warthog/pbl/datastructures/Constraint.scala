package org.warthog.pbl.datastructures



/**
 *  Abstract base class for a pseudo-boolean constraints
 *  and cardinality constraints
 *  Form of a pseudo-boolean constraint: a_1x_1 + a_2x_2 + ... + a_nx_n >= degree
 *  Form of a cardinality constraint: x_1 + x_2 + x_3 + ... + x_n >= degree
 *  Note that all constraints are normalized which means:
 *  - all coefficients a_i > 0
 *  - degree > 0
 *  - relational operator: >=
 */
abstract class Constraint (){
  //left-hand side of the constraint
  var terms :  List[PBLTerm]
  //right-hand side of the constraint
  var degree : BigInt


  def this(terms: List[PBLTerm], degree: BigInt){
    this()
    this.terms = terms
    this.degree = degree
    normalize()
  }

  /**
   * String representation of the constraint
   * @return the string representation
   */
  override def toString = terms.map(_.toString()+ " + ").foldLeft("")(_+_).dropRight(2) + ">= " + degree

  /**
   * Normalize the constraint by eliminating all negative coefficients
   */
  def normalize() = {
    terms.map { t =>
      if (t.a < 0) {
        t.a = t.a.abs
        t.l = t.l.negate
        degree += t.a
      }
    }
  }

  /**
   * Returns a copy of the constraint
   * @return the copied constraint
   */
  def copy: Constraint

  def *(x: BigInt) =  {
    terms.map(_.a *= x)
    degree *= x
  }

  /**
   * Initialize the watched literals.
   * @return the state of the constraint
   */
  def initWatchedLiterals: State.Value
}

object State extends Enumeration{
  type State = Value
  val UNIT, EMPTY, SAT, SUCCESS = Value
}
