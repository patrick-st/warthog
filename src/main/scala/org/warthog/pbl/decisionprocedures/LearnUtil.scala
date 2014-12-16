package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 *
 */
object LearnUtil {

  /**
   * Method to learn clauses
   * @param conflict the conflict
   * @param stack the assigned variables and their reasons
   * @param level the current level
   * @return the new clause to learn
   */
  def learnClause(conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int) =
    learn(reduce2Clause, reduce2Clause, resolve, conflict, stack, level)

  /**
   * Generic function to provide three different learn methods
   * 1. clause learning
   * 2. pseudo-boolean constraint learning
   * 3. cardinality constraint learning
   * For more informations see: Donald Chai, Andreas Kuehlmann: A Fast Pseudo-Boolean Constraint Solver
   * @param reduce1
   * @param reduce2
   * @param resolve
   * @param conflict the conflict
   * @param stack the assigned variables and their reasons
   * @param level the current level
   * @return the new clause to learn
   */
  private def learn(reduce1: (Constraint, PBLVariable) => Constraint, reduce2: (Constraint, PBLVariable) => Constraint,
                    resolve: (Constraint, Constraint, PBLVariable) => Constraint,
                    conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int): Constraint = {

    var c1 = conflict
    //resolve the conflict until the resolvent is 1UIP
    while (!stack.isEmpty) {
      val v = stack.pop()
      c1 = reduce1(c1, v)
      val c2 = reduce2(v.reason, v)
      c1 = resolve(c1, c2, v)
      v.unassign()
      if (is1UIP(c1, level)) {
        return c1
      }
    }
    null
  }

  /**
   * Reduce the given constraint to a clause
   * @param c the constraint to reduce
   * @param v this variable don't have to be removed
   *          because over this variable will be resolved
   * @return the reduced clause
   */
  private def reduce2Clause(c: Constraint, v: PBLVariable): Constraint = {
    if (c.isInstanceOf[PBLCardinalityConstraint] && c.asInstanceOf[PBLCardinalityConstraint].degree == 1) {
      c.copy
    } else {
      val newTerms = ListBuffer[PBLTerm]()
      c.terms.foldLeft(newTerms) { (buffer, term) =>
        if (term.l.evaluates2False || term.l.v == v) {
          buffer += new PBLTerm(1, term.l.copy)
        }
        buffer
      }
      new PBLCardinalityConstraint(newTerms.toList, 1)
    }
  }

  /**
   * Checks if the constraints are resolvable or not
   * @param c1 first constraint
   * @param c2 second constraint
   * @return true if c1 and c2 are resolvable else false
   */
  private def isResolvable(c1: Constraint, c2: Constraint, v: PBLVariable): Boolean = {
    var t1: PBLLiteral = null
    var t2: PBLLiteral = null
    val existsC1 = c1.terms.exists { t => t1 = t.l; t.l.v == v}
    val existsC2 = c2.terms.exists { t => t2 = t.l; t.l.v == v}
    existsC1 && existsC2 && t1.phase != t2.phase
  }

  /**
   * Computes the resolvent of two given constraints
   * @param c1 first constraint
   * @param c2 second constraint
   * @param v the variable to resolve
   * @return the resolvent
   */
  private def resolve(c1: Constraint, c2: Constraint, v: PBLVariable) = {
    if (isResolvable(c1, c2, v)) {
      val a1 = c1.terms.find(_.l.v == v).get.a
      val a2 = c2.terms.find(_.l.v == v).get.a
      val lcd = (a1 * a2) / a1.gcd(a2)
      val coeff1 = lcd / a1
      val coeff2 = lcd / a2
      //multiply the constraints with the coefficients
      val c1_ = c1.copy
      c1_ * coeff1
      val c2_ = c2.copy
      c2_ * coeff2
      for (t <- c2_.terms) {
        c1_.terms.find(_.l.v == t.l.v) match {
          case Some(term) => {
            if (term.l.phase == t.l.phase) {
              term.a += t.a
            } else {
              c2_.degree -= t.a
              term.a -= t.a
              if (term.a == 0) {
                //delete the term
                c1_.terms = c1_.terms.filter(_.l.v != t.l.v)
              }
            }
          }
          case None => c1_.terms :+= t
        }
      }
      //check if constraint is cardinality or not
      if (c1_.terms.forall(_.a.abs == c1_.terms(0).a.abs))
        new PBLCardinalityConstraint(c1_.terms, c1_.degree + c2_.degree)
      else {
        var c: Constraint = new PBLConstraint(c1_.terms, c1_.degree + c2_.degree)
        c.reduce()
        if(c.terms.forall(_.a.abs == c.terms(0).a.abs))
          c = new PBLCardinalityConstraint(c.terms, c.degree)
        c
      }
    } else {
      c1
    }
  }

  /**
   * Check if the given constraint is 1UIP or not
   * Note: Currently only applicable for clauses
   * Maybe the method can be adapted for pseudo-boolean constraints
   * @param c1
   * @param level
   * @return
   */
  private def is1UIP(c1: Constraint, level: Int) = {
    c1.terms.filter(_.l.v.level == level).size == 1
  }
}

