package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures.{PBLTerm, Constraint}

/**
 * Interface for Pseudo-Boolean
 */
trait OptimisationProcedure {

  /**
   * Add a constraint which has to be solved
   * @param c the constraint
   */
  def add(c: Constraint)

  /**
   * Add a several constraints which have to be solved
   * @param constraints the constraints
   */
  def add(constraints: List[Constraint]): Unit = constraints.map(add(_))

  /**
   * Solve the added and additional given constraints
   * @param objectiveFunction to minimize
   * @return None if the instance is unsat else the optimum
   */
  def solve(objectiveFunction: List[PBLTerm]): Option[BigInt]

  /**
   * Reset the optimiser to initial state
   */
  def reset()
}
