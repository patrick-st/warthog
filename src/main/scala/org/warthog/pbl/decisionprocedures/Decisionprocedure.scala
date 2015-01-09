package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures.Constraint
import org.warthog.pl.decisionprocedures.satsolver.Model


/**
 * Interface for Pseudo-Boolean Solvers
 */
trait DecisionProcedure {

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
   * Solve the added and additional given constraints
   * @param constraints additional constraints to solve
   * @return the constants Solver.SAT, Solver.UNSAT or Solver.UNKNOWN
   */
  def solve(constraints: List[Constraint]): Int

  /**
   * Reset the solver to initial state.
   */
  def reset()

  /**
   * Mark a solver's internal stack position.  Executing
   * {{{
   * Solver.add(f0)
   * Solver.mark()
   * Solver.add(a1)
   * Solver.add(a2)
   * Solver.undo()
   * }}}
   * will set the solver back into the state after adding `f0`
   */
  def mark()

  /**
   * Undo all the additions until the last marked position.
   */
  def undo()

  def getModel: Option[Model]
}

