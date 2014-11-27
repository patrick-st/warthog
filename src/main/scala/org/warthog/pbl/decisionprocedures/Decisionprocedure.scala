package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures.Constraint

/**
 * Interface for Pseudo-Boolean Solvers
 */
trait Decisionprocedure {
  /**
   * Add a constraint which has to be solved
   * @param c the constraint
   */
  def add(c: Constraint): Unit

  /**
   * Add a several constraints which have to be solved
   * @param constraints the constraints
   */
  def add(constraints: List[Constraint]): Unit

  /**
   * Solve the added constraints and additional the given constraints
   * @param constraints to solve additional
   * @return true if all constraints are satisfiable else false
   */
  def solve(constraints: List[Constraint]): Boolean

  /**
   * Reset the solver to initial state.
   * The constraints, added by the add-functions, aren't removed
   */
  def reset(): Unit
}

