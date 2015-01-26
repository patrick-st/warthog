/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler & Rouven Walter
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.warthog.pl.optimization.maxsat.partialWeighted

import org.warthog.pl.decisionprocedures.satsolver.{Model, Solver}
import org.warthog.pl.datastructures.cnf.{PLLiteral, MutablePLClause, ImmutablePLClause}
import org.warthog.generic.formulas.Formula
import org.warthog.pl.formulas.{PLAtom, PL}
import org.warthog.pl.optimization.maxsat.MaxSATHelper
import org.warthog.pl.generators.pbc.PBCtoSAT
import org.warthog.generic.datastructures.cnf.ClauseLike

/**
 * Binary Search algorithm for Partial Weighted MaxSAT.
 */
class BinarySearch(satSolver: Solver, pbcGenerator: PBCtoSAT) extends PartialWeightedMaxSATSolver() {

  private var workingModel: Model = null

  override def name = "BinarySearch"

  override def reset() {
    super.reset()
    satSolver.reset()
  }

  override def addHardConstraint(clause: ClauseLike[PL, PLLiteral]) {
    satSolver.add(clause)
  }

  override def markHardConstraints() {
    satSolver.mark()
  }

  def undoHardConstraints() {
    satSolver.undo()
  }

  override protected def solveMinUNSATImpl(softClauses: Traversable[ClauseLike[PL, PLLiteral]], weights: List[Long]): Long = {
    satSolver.mark() /* Mark to remove all added clauses after solving */
    val result = solveMinUNSATImplHelper(softClauses.map(c => new MutablePLClause(c.literals)).toList, weights)
    satSolver.undo()
    result
  }

  private def solveMinUNSATImplHelper(softClauses: List[MutablePLClause], weights: List[Long]): Long = {
    // Adding blocking variables to each soft clause
    var blockingVarsIndex = 0
    val blockingVars = new Array[PLAtom](softClauses.size)
    for (softClause <- softClauses) {
      val v = new PLAtom(BinarySearch.BLOCKING_VARIABLE_PREFIX + blockingVarsIndex)
      blockingVars(blockingVarsIndex) = v
      softClause.push(new PLLiteral(v, true))
      blockingVarsIndex += 1
    }

    for (c <- softClauses)
      satSolver.add(c)
    sat()

    var lb = 0L /* Initial lower bound */
    var ub = cost(softClauses, weights, blockingVars, workingModel) /* Initial upper bound */
    var mid = (ub - lb) / 2 + lb /* Initial mid */

    while (lb < ub) {
      // Is the instance satisfiable with lower or equal many blocking variables satisfied than 'mid'?
      if (sat(pbcGenerator.le(weights.zip(blockingVars.map(bv => new PLLiteral(bv, true))), mid)))
        ub = cost(softClauses, weights, blockingVars, workingModel) /* New upper bound */
      else
        lb = mid + 1 /* New lower bound */
      mid = ((ub - lb) / 2) + lb /* New middle */
    }
    model = Some(workingModel.
      filterNot(BinarySearch.BLOCKING_VARIABLE_PREFIX).
      filterNot(PBCtoSAT.DEFAULT_PREFIX))
    ub
  }

  override protected def areHardConstraintsSatisfiable() = {
    satSolver.sat() == Solver.SAT
  }

  private def sat(clauses: Set[ImmutablePLClause] = Set.empty): Boolean = {
    satSolver.mark()
    for (c <- clauses)
      satSolver.add(c)
    val isSAT = satSolver.sat() == Solver.SAT
    if (isSAT)
      workingModel = satSolver.getModel().get
    satSolver.undo()
    isSAT
  }

  private def cost(softClauses: List[MutablePLClause], weights: List[Long],
                   blockingVars: Array[PLAtom], model: Model): Long = {
    var cost = 0L
    val posVars = model.positiveVariables
    for (i <- 0 until softClauses.size)
      if (posVars.contains(blockingVars(i)))
        cost += weights(i)
    cost
  }
}

object BinarySearch {
  val BLOCKING_VARIABLE_PREFIX = "BV___"
}
