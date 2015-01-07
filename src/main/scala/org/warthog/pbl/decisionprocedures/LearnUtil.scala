package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 *
 */
object LearnUtil {

  def learnPBConstraint(conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int) = {
    learn(reduce,resolve,conflict,stack,level)
  }

  /**
   * Generic function to provide three different learn methods
   * 1. clause learning
   * 2. pseudo-boolean constraint learning
   * 3. cardinality constraint learning
   * For more informations see: Donald Chai, Andreas Kuehlmann: A Fast Pseudo-Boolean Constraint Solver
   * @param reduce
   * @param resolve
   * @param conflict the conflict
   * @param stack the assigned variables and their reasons
   * @param level the current level
   * @return the new clause to learn
   */
  private def learn(reduce: (Constraint,Constraint, PBLVariable) => Constraint,
                    resolve: (Constraint, Constraint, PBLVariable) => Constraint,
                    conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int): Constraint = {

    var c1 = conflict
    val vars2Unassign = mutable.Set[PBLVariable]()
    //resolve the conflict until the resolvent is 1UIP
    while (!stack.isEmpty) {
      val v = stack.pop()
      if(v.reason == null || stack.isEmpty) {
        v.unassign()
        return c1
      }
      if (isResolvable(c1, v.reason, v)) {
        val c2 = reduce(c1,v.reason, v)
        c1 = resolve(c1, c2, v)
      }
      v.unassign()
      if (is1UIP(c1, level)) {
        vars2Unassign.map(_.unassign())
        return c1
      }
    }
    null
  }


  /**
   * Generic function to provide three different learn methods
   * 1. clause learning
   * 2. pseudo-boolean constraint learning
   * 3. cardinality constraint learning
   * For more informations see: Donald Chai, Andreas Kuehlmann: A Fast Pseudo-Boolean Constraint Solver
   * @param conflict the conflict
   * @param stack the assigned variables and their reasons
   * @param level the current level
   * @return the new clause to learn
   */
  def learnClause(conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int): Constraint = {

    var c1 = conflict
    //resolve the conflict until the resolvent is 1UIP
    while (!stack.isEmpty) {
      val v = stack.pop()
      if (isResolvable(c1, v.reason, v)) {
        c1 = reduce2Clause(c1, v)
        val c2 = reduce2Clause(v.reason, v)
        c1 = resolve(c1, c2, v)
      }
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

  private def reduce(c1: Constraint, c2: Constraint, v: PBLVariable): Constraint = {
    val c1_ = c1.copy
    val c2_ = c2.copy
    var slack1 = c1_.getSlack()
    var slack2 = c2_.getSlack()
    var coeffPair = computeScalar(c1_,c2_,v)
    var coeff1 = coeffPair._1
    var coeff2 = coeffPair._2
    while(slack1*coeff1 + slack2*coeff2 >= 0){
      //the resolved constraints would lead a constraint which is not in conflict to the current variable assignment
      val terms = c2_.terms.filter(t => (t.l.evaluates2True || t.l.v.state == State.OPEN) && t.l.v != v)
      if(terms.isEmpty)
        return reduce2Clause(c2,v)
      val term2remove = terms.minBy(_.a)
      //remove the term and update the degree
      c2_.terms = c2_.terms.filter(_ != term2remove)
      c2_.degree -= term2remove.a
      //saturation step
      c2_.reduce()
      if(c2_.degree <= 0)
        return reduce2Clause(c2,v)
      //update the slacks and the coefficients
      slack2 = c2_.getSlack()
      coeffPair = computeScalar(c1_,c2_,v)
      coeff1 = coeffPair._1
      coeff2 = coeffPair._2
    }
    c2_
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
    val coeffPair = computeScalar(c1, c2, v)
    val coeff1 = coeffPair._1
    val coeff2 = coeffPair._2
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
      if (c.terms.forall(_.a.abs == c.terms(0).a.abs))
        c = new PBLCardinalityConstraint(c.terms, c.degree)
      c
    }
  }

  private def computeScalar(c1: Constraint, c2: Constraint, v: PBLVariable): (BigInt, BigInt) = {
    val a1 = c1.terms.find(_.l.v == v).get.a
    val a2 = c2.terms.find(_.l.v == v).get.a
    val lcd = (a1 * a2) / a1.gcd(a2)
    (lcd / a1, lcd / a2)
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
    c1 match {
      case cardinality: PBLCardinalityConstraint =>
        cardinality.terms.filter(_.l.v.level == level).size == cardinality.degree
      case c: PBLConstraint => c.terms.filter(_.l.v.level == level).size == 1
    }
  }
}

