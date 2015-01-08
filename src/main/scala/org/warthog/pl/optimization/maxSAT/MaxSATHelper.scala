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

package org.warthog.pl.optimization.maxsat

import org.warthog.pl.datastructures.cnf.{PLLiteral, MutablePLClause, ImmutablePLClause}
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import collection.mutable
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.pl.decisionprocedures.satsolver.Model
import org.warthog.generic.datastructures.cnf.ClauseLike

object MaxSATHelper {
  /**
   * Calculate the costs of unsatisfied clause weights.
   *
   * Assumption: Model is a complete variable assignment.
   *
   * @param clauses
   * @param weights
   * @param model
   * @return
   */
  def cost(clauses: List[ClauseLike[PL, PLLiteral]], weights: List[Long], model: Model): Long = {
    var benefit = 0L
    val posVars = model.positiveVariables
    val negVars = model.negativeVariables
    for (weightedClause <- weights.zip(clauses))
      if (weightedClause._2.literals.exists(lit =>
        lit.phase && posVars.contains(lit.variable)
          || !lit.phase && negVars.contains(lit.variable)))
        benefit += weightedClause._1
    weights.sum - benefit
  }
}
