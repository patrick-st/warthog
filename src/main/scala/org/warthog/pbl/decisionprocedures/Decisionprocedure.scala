package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures.{PBLVariable, Constraint}
import org.warthog.pl.decisionprocedures.satsolver.Model

import scala.collection.mutable

/**
 * Interface for Pseudo-Boolean Solvers
 */
trait DecisionProcedure {

  /**
   * The variables of the instance to solve
   */


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
   * @param constraints additional constraints to solve
   * @return true if all constraints are satisfiable else false
   */
  def solve(constraints: List[Constraint]): Int

  /**
   * Reset the solver to initial state.
   * The constraints, added by the add-functions, aren't removed
   */
  def reset()

  def mark()

  def undo()

  def getModel(): Option[Model]
}

