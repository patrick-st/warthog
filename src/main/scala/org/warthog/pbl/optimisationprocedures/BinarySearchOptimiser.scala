package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures.{PBLConstraint, PBLCardinalityConstraint, Constraint, PBLTerm}
import org.warthog.pbl.decisionprocedures.{CDCLLike, DecisionProcedure}

/**
 * Implementation of an optimiser, which uses binary search to compute the optimum
 */
class BinarySearchOptimiser(val solver: DecisionProcedure) extends OptimisationProcedure {

  //the original parsed minimisation function of the opb-file
  var minimizeFunction: List[PBLTerm] = null
  //the computed maximisation function out of the minimisation function
  var maximizeFunction: Constraint = null
  // the optimum computed by the minimisation function
  var minOptimum: BigInt = null
  // the optimum computed by the maximisation function
  var maxOptimum: BigInt = null

  def add(c: Constraint) = solver.add(c)

  def reset() {
    solver.reset()
    minimizeFunction = null
    maximizeFunction = null
    minOptimum = null
    maxOptimum = null
  }

  def solve(objectiveFunction: List[PBLTerm]): Option[BigInt] = {
    //exchange variables
    objectiveFunction.map { t =>
      t.l.v = solver.variables.getOrElseUpdate(t.l.v.ID, t.l.v)
    }

    //compute the minimize and maximize functions
    minimizeFunction = objectiveFunction.foldLeft(List[PBLTerm]())(_ :+ _.copy)
    //compute the maximization function
    val rhs: BigInt = objectiveFunction.filter(_.a > 0).map(_.a).sum
    //multiply with -1 to get the maximization function
    objectiveFunction.map(_.a *= -1)
    //check if objective function is cardinality
    if (objectiveFunction.forall(_.a.abs == objectiveFunction(0).a.abs)) {
      //Note: set the removable flag to true
      maximizeFunction = new PBLCardinalityConstraint(objectiveFunction, -rhs, true)
    } else {
      maximizeFunction = new PBLConstraint(objectiveFunction, -rhs, true)
    }

    //determine upper and lower bound for binary search
    var upper = maximizeFunction.terms.map(_.a).sum
    var lower = maximizeFunction.degree
    var middle = (lower + upper) / 2
    //enforce  objectiveFunction >= middle
    var lowerBoundConstraint = maximizeFunction.copy
    lowerBoundConstraint.degree = middle

    //enforce objectiveFunction <= upper
    var upperBoundConstraint = computeUpperBoundConstraint(upper)

    while (lower <= upper) {
      if (solver.solve(List[Constraint](lowerBoundConstraint, upperBoundConstraint))) {
        //update max and minOptimum
        maxOptimum = maximizeFunction.terms.filter(_.l.evaluates2True).map(_.a).sum
        minOptimum = minimizeFunction.filter(_.l.evaluates2True).map(_.a).sum
        //update the new lower bound
        lower = maxOptimum + 1
      } else {
        //update the new upper bound
        upper = middle - 1
      }
      middle = (lower + upper) / 2
      //update the constraints
      lowerBoundConstraint = maximizeFunction.copy
      lowerBoundConstraint.degree = middle
      upperBoundConstraint = computeUpperBoundConstraint(upper)
      solver.reset()
    }

    //return the optimum
    if (minOptimum == null) {
      None
    } else {
      Some(minOptimum)
    }
  }

  /**
   * Method computes the constraint to enforce objective function <= upper bound
   * @param degree the upper bound
   * @return the constraint
   */
  private def computeUpperBoundConstraint(degree: BigInt): Constraint = {
    val terms = maximizeFunction.terms.foldLeft(List[PBLTerm]())(_ :+ _.copy)
    terms.map(_.a *= -1)
    //check if constraint is cardinality or not
    if (maximizeFunction.terms.forall(_.a.abs == maximizeFunction.terms(0).a.abs)) {
      new PBLCardinalityConstraint(terms, -degree)
    } else {
      new PBLConstraint(terms, -degree)
    }
  }
}
