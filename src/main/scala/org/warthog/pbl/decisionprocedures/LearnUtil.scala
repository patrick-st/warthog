package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 *
 */
object LearnUtil {

  def learnCardinalityConstraint(conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int) = {
    learn((c: Constraint, v: PBLVariable) => c, reduce, reduceToCardinality, resolve, conflict, stack, level)
  }

  /**
   * Method to learn PB Constraints
   * @param conflict to analyze
   * @param stack of assigned variables in reversed order
   * @param level current decision level
   * @return the constraint to learn
   */
  def learnPBConstraint(conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int) = {
    learn((c: Constraint, v: PBLVariable) => c, reduce, identity, resolve, conflict, stack, level)
  }

  /**
   * Method to learn clauses
   * @param conflict to analyze
   * @param stack of assigned variables in reversed order
   * @param level current decssion level
   * @return the clause to learn
   */
  def learnClause(conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int) = {
    learn(reduce2Clause, reduce2Clause, identity, resolveClauses, conflict, stack, level)
  }

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
  private def learn(reduce1: (Constraint, PBLVariable) => Constraint,
                    reduce2: (Constraint, Constraint, PBLVariable) => Constraint,
                    reduce3: (Constraint) => Constraint,
                    resolve: (Constraint, Constraint, PBLVariable) => Constraint,
                    conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int): (Constraint, Option[Int]) = {

    var c1 = conflict
    //resolve the conflict until the resolvent is 1UIP
    while (stack.nonEmpty) {
      val v = stack.pop()
      //if no resolve step is possible return the computed constraint
      if (v.reason == null) {
        stack.push(v)
        return (reduce3(c1),None)
      }
      c1 = reduce1(c1, v)
      if (isResolvable(c1, v.reason, v)) {
        val c2 = reduce2(c1, v.reason, v)
        c1 = resolve(c1, c2, v)
      }
      v.unassign()
      is1UIP(c1,level) match {
        case Some(unitLevel) => return (reduce3(c1), Some(unitLevel))
        case None =>
      }

    }
    (reduce3(c1),None)
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
   * Wrapper for reduce2Clause(c: Constraint, v: PBLVariable)
   * @param c1 is ignored
   * @param c2 the constraint to reduce
   * @param v this variable don't have to be removed
   *          because over this variable will be resolved
   * @return the reduced clause c2
   */
  private def reduce2Clause(c1: Constraint, c2: Constraint, v: PBLVariable): Constraint = reduce2Clause(c2, v)

  /**
   * Method guarantees that the resolved constraint will be unsatisfied under the current assignment.
   * The resolved constraint has to be empty during all resolve steps.
   * If c2 is oversatisfied (slack > 0) then the resolvent can be unit or sat.
   * This has to be prevented because the resolvent wouldn't be in conflict to the current variable assignment.
   * So the method reduces the constraint c2 (and thus it's slack) until the resulting resolvent will be empty.
   * For more information see: Hossein M. Sheini and Karem A. Sakallah: Pueblo: A Hybrid Pseudo-Boolean SAT Solver
   * @param c1 the current resolved constraint
   * @param c2 will be resolved with c1
   * @param v the variable to resolve over
   * @return the reduced constraint c2
   */
  private def reduce(c1: Constraint, c2: Constraint, v: PBLVariable): Constraint = {
    val c1_ = c1.copy
    val c2_ = c2.copy
    val slack1 = c1_.getSlack
    var slack2 = c2_.getSlack
    //get the scalars which are necessary to eliminate the variable v
    var coeffPair = computeScalar(c1_, c2_, v)
    var coeff1 = coeffPair._1
    var coeff2 = coeffPair._2
    //reduce c2_ until the resolvent will be empty
    while (slack1 * coeff1 + slack2 * coeff2 >= 0) {
      //only positive of open terms can be removed
      val removableTerms = c2_.terms.filter(t => (t.l.evaluates2True || t.l.v.state == State.OPEN) && t.l.v != v)
      //if no removable term exists, reduce the constraint to a clause
      if (removableTerms.isEmpty)
        return reduce2Clause(c2, v)
      val term2remove = removableTerms.minBy(_.a)
      //remove the term and update the degree
      c2_.terms = c2_.terms.filter(_ != term2remove)
      c2_.degree -= term2remove.a
      //saturation step
      c2_.saturation()
      //if a tautology arises, reduce the constraint to a clause
      if (c2_.degree <= 0)
        return reduce2Clause(c2, v)
      //update the slacks and the coefficients
      slack2 = c2_.getSlack
      coeffPair = computeScalar(c1_, c2_, v)
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
   * Computes the resolvent of two given clauses
   * Note: Only applicable for clauses
   * @param c1 first clause
   * @param c2 second clause
   * @param v the variable to resolve
   * @return the resolvent
   */
  private def resolveClauses(c1: Constraint, c2: Constraint, v: PBLVariable) = {
    val newTerms = (c1.terms.filter(_.l.v != v) union c2.terms.filter(_.l.v != v)).distinct.foldLeft(List[PBLTerm]())(_ :+ _.copy)
    new PBLCardinalityConstraint(newTerms, 1)
  }

  /**
   * Computes the resolvent of two given constraints.
   * This method is applicable for all kind of constraints.
   * @param c1 first constraint
   * @param c2 second constraint
   * @param v the variable to resolve
   * @return the resolvent
   */
  private def resolve(c1: Constraint, c2: Constraint, v: PBLVariable) = {
    //compute the scalars to eliminate the variable v
    val coeffPair = computeScalar(c1, c2, v)
    val coeff1 = coeffPair._1
    val coeff2 = coeffPair._2
    //multiply the constraints with the coefficients
    val c1_ = c1.copy
    c1_ * coeff1
    val c2_ = c2.copy
    c2_ * coeff2
    //add all terms with equal variables accordingly
    for (t <- c2_.terms) {
      c1_.terms.find(_.l.v == t.l.v) match {
        case Some(term) =>
          if (term.l.phase == t.l.phase) {
            //5 x1 + 2 x1 = 8 x1
            term.a += t.a
          } else {
            //5 x1 + 2 ~x1 = 5 x1 - 2 x1 = 3 x1
            // note: 2 ~x1 can be only transformed to -2 x1 by updating the degree accordingly
            c2_.degree -= t.a
            term.a -= t.a
            if (term.a == BigInt(0)) {
              //delete the term
              c1_.terms = c1_.terms.filter(_.l.v != t.l.v)
            }
          }
        case None => c1_.terms :+= t
      }
    }
    if(c1_.terms.isEmpty) {
      //it can happen that an empty constraint will be computed
      //if an empty constraint is computed generate an empty CardinalityConstraint
      val term = new PBLTerm(1,new PBLLiteral(c1.terms(0).l.v))
      new PBLCardinalityConstraint(List[PBLTerm](term),BigInt(2))
    } else {
      //check if constraint is cardinality or not
      if (c1_.terms.forall(_.a.abs == c1_.terms(0).a.abs))
        new PBLCardinalityConstraint(c1_.terms, c1_.degree + c2_.degree)
      else {
        var c: Constraint = new PBLConstraint(c1_.terms, c1_.degree + c2_.degree)
        c.saturation()
        if (c.terms.forall(_.a.abs == c.terms(0).a.abs))
          c = new PBLCardinalityConstraint(c.terms, c.degree)
        c
      }
    }
  }

  /**
   * Compute the two scalars a and b to eliminate the variable v so that
   * a*c1 + b*c2 lead to a constraint with eliminated v
   * @param c1
   * @param c2
   * @param v
   * @return (a,b)
   */
  private def computeScalar(c1: Constraint, c2: Constraint, v: PBLVariable): (BigInt, BigInt) = {
    val a1 = c1.terms.find(_.l.v == v).get.a
    val a2 = c2.terms.find(_.l.v == v).get.a
    val lcd = (a1 * a2) / a1.gcd(a2)
    (lcd / a1, lcd / a2)
  }


  /**
   * Check if the given constraint is 1UIP or not
   * @param c1 the constraint to check
   * @param level the current decision level
   * @return Some(backtrackLevel) where the constraint is unit or None if the constraint is not unit
   */
  private def is1UIP(c1: Constraint, level: Int): Option[Int] = {
    var backtrackLevel = level-1
    c1 match {
      case cardinality: PBLCardinalityConstraint => {
        if (cardinality.degree == BigInt(1)) {
          //case clause:
          if(cardinality.terms.count(_.l.v.level == level) == 1) {
            //check if constraint is initial unit
            if (cardinality.terms.size == 1)
              return Some(0)
            Some(cardinality.terms.sortBy(_.l.v.level).reverse.tail.head.l.v.level)
          } else None
        } else {
          //case cardinality constraint
          var possibleBacktrackLevel: List[Int] = c1.terms.map(_.l.v.level).:+(0).distinct.filter(_ != -1).sorted.reverse
          if(possibleBacktrackLevel.head == level)
            possibleBacktrackLevel = possibleBacktrackLevel.tail

          while (possibleBacktrackLevel.nonEmpty) {
            backtrackLevel = possibleBacktrackLevel.head
            val levelPartition = cardinality.terms.partition(t => t.l.v.level > backtrackLevel || t.l.v.state == State.OPEN)
            //number of open variables after backtracking
            val numberOfOpenTerms = levelPartition._1.size

            val evaluatesPartition = levelPartition._2.partition(_.l.evaluates2False)
            //number of false terms after backtracking
            val falseTerms = evaluatesPartition._1
            //number of true terms after backtracking
            val numberOfTrueTerms = evaluatesPartition._2.size
            if ((BigInt(numberOfOpenTerms) + BigInt(numberOfTrueTerms)) == cardinality.degree && falseTerms.exists(_.l.v.level == backtrackLevel)) {
              return Some(backtrackLevel)
            }
            possibleBacktrackLevel = possibleBacktrackLevel.tail
          }
          None
        }
      }
      case c: PBLConstraint => {
        var possibleBacktrackLevel: List[Int] = c1.terms.map(_.l.v.level).:+(0).distinct.filter(_ != -1).sorted.reverse
        if(possibleBacktrackLevel.head == level)
          possibleBacktrackLevel = possibleBacktrackLevel.tail

        //collect alle level where the constraint is unit
        var listUnitLevels = List[Int]()
        val copiedTerms = c.terms.map(_.copy)
        while(possibleBacktrackLevel.nonEmpty){
          backtrackLevel = possibleBacktrackLevel.head
          val partition = copiedTerms.partition(_.l.v.level > backtrackLevel)
          val futureUnassignedTerms = partition._1
          val fixTerms = partition._2
          val futureSlack = fixTerms.filter(!_.l.evaluates2False).map(_.a).sum + futureUnassignedTerms.map(_.a).sum - c.degree
          if(futureSlack >= 0 && copiedTerms.exists(t => t.l.v.state == State.OPEN && t.a > futureSlack)){
            listUnitLevels :+= backtrackLevel
          }
          possibleBacktrackLevel = possibleBacktrackLevel.tail
        }
        if(listUnitLevels.isEmpty)
          None
        else
          Some(listUnitLevels.min)
      }

    }
  }

  private def reduceToCardinality(c: Constraint):Constraint = {
    val cardinality: Constraint = c match {
      case card: PBLCardinalityConstraint => card.copy
      case constr: PBLConstraint => constr.toCardinalityConstraint
    }
    //check if the cardinality constraint is not empty under current assignment
    if(cardinality.getSlack >= 0){
      //if cardinality is not empty reduce the constraint to clause
      cardinality.terms = cardinality.terms.filter(_.l.evaluates2False)
      cardinality.degree = 1
    }
    cardinality
  }
}

