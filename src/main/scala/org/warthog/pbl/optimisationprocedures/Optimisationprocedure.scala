package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures.{PBLTerm, Constraint}
import org.warthog.pl.decisionprocedures.satsolver.Model

/**
 * Interface for Pseudo-Boolean Optimiser
 */
trait OptimisationProcedure {

  /**
   * the computed optimum of the minimization function
   */
  var minOptimum: BigInt

  /**
   * Add a constraint which has to be solved
   * @param c the constraint
   */
  def add(c: Constraint)

  /**
   * Add several constraints which have to be solved
   * @param constraints the constraints
   */
  def add(constraints: List[Constraint]): Unit = constraints.map(add(_))

  /**
   * Solve the added constraints
   * @param objectiveFunction to minimize
   * @return None if the instance is unsat else the optimum
   */
  def min(objectiveFunction: List[PBLTerm]): Option[BigInt]

  /**
   * Reset the optimiser to initial state
   */
  def reset()

  def getModel: Option[Model]
}
