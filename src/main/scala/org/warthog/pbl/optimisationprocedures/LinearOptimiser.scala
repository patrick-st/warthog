package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures.{PBLConstraint, PBLCardinalityConstraint, Constraint, PBLTerm}
import org.warthog.pbl.decisionprocedures.{DecisionProcedure, CDCLLike}
import org.warthog.pl.decisionprocedures.satsolver.{Solver, Model}
import org.warthog.pl.formulas.PLAtom

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
   * Main entry point for optimizing the added constraints by linear search
   * the objective function will be minimized
   */
  def solve(objectiveFunction: List[PBLTerm]): Option[BigInt] = {

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

    solver.mark()
    var model: Model = null
    //start to optimize
    while (solver.solve(List[Constraint](maximizeFunction.copy)) == Solver.SAT) {
      //compute the max optimum
      model = solver.getModel().get
      maxOptimum = evaluateObjectiveFunction(maximizeFunction.terms, model)
      //update the objective function
      maximizeFunction.degree = maxOptimum + 1
      solver.undo()
      solver.mark()
    }
    solver.undo()

    minOptimum = evaluateObjectiveFunction(minimizeFunction,model)

    //return the optimum
    if (minOptimum == null) {
      None
    } else {
      Some(minOptimum)
    }
  }

  private def evaluateObjectiveFunction(function: List[PBLTerm], model: Model): BigInt = {
   val filter =  function.filter(t => t.l.phase && model.positiveVariables.contains(PLAtom(t.l.v.name)) ||
      !t.l.phase && model.negativeVariables.contains(PLAtom(t.l.v.name)))
    filter.map(_.a).sum
  }
}
