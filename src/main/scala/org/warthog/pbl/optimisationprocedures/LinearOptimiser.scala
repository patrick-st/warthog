package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures.{PBLConstraint, PBLCardinalityConstraint, Constraint, PBLTerm}
import org.warthog.pbl.decisionprocedures.{DecisionProcedure, CDCLLike}

/**
 * Implementation of an optimiser, which uses linear search to compute the optimum
 */
class LinearOptimiser(val solver: DecisionProcedure) extends OptimisationProcedure {

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

  /**
   * Main entry point for optimizing the given instance by linear search
   */
  def solve(objectiveFunction: List[PBLTerm]): Option[BigInt] = {
    //exchange variables
    objectiveFunction.map { t =>
      t.l.v = solver.variables.getOrElseUpdate(t.l.v.ID, t.l.v)
    }

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

    //start to optimize
    while (solver.solve(List[Constraint](maximizeFunction.copy))) {
      //compute the max and min optimum
      maxOptimum = maximizeFunction.terms.filter(_.l.evaluates2True).map(_.a).sum
      minOptimum = minimizeFunction.filter(_.l.evaluates2True).map(_.a).sum
      //update the objective function
      maximizeFunction.degree = maxOptimum + 1
      //reset the solver
      solver.reset()
    }

    //return the optimum
    if (minOptimum == null) {
      None
    } else {
      Some(minOptimum)
    }
  }
}
