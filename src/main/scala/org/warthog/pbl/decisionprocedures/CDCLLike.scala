package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures._
import org.warthog.pl.decisionprocedures.satsolver.{Model, Solver}
import org.warthog.pl.formulas.PLAtom
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Implements a CDCL like approach to solve pseudo-boolean constraints
 */
class CDCLLike extends DecisionProcedure {
  var variables = mutable.HashMap[Int, PBLVariable]()
  var constraints = List[Constraint]()
  var level = 0
  var stack = mutable.Stack[PBLVariable]()
  var units = mutable.HashSet[Constraint]()
  var marks: List[Int] = Nil
  var lastState = Solver.UNKNOWN
  var numberOfLearnedClauses = 0

  /**
   * Adding a constraint to the constraints
   * @param c the constraint to add
   */
  def add(c: Constraint) {
    //exchange variables if necessary
    c.terms.map { t =>
      t.l.v = variables.getOrElseUpdate(t.l.v.ID, t.l.v)
    }

    constraints :+= c

    //update the state
    if (lastState != Solver.UNSAT)
      lastState = Solver.UNKNOWN
  }

  /**
   * Method is a wrapper for the private solve-method.
   * Solving process starts only if the solver state is unknown.
   * After solving a cleanUp method prepares the solver for new solving calls
   * @param constraints additional constraints to solve
   * @return the constants Solver.SAT, Solver.UNSAT or Solver.UNKNOWN
   */
  def solve(constraints: List[Constraint]): Int = {
    if (constraints.nonEmpty)
      add(constraints)

    //solve only if solver state is unknown
    if (lastState == Solver.UNKNOWN) {
      unassignVariables()
      initConstraints()
      /* only if no empty constraint is included in the constraint set the solver starts*/
      if (lastState == Solver.UNKNOWN) {
        solve()
      }
      cleanUp()
    }
    lastState
  }

  /**
   * Method resets the solver to the initial state
   */
  def reset() {
    variables.clear()
    constraints = List[Constraint]()
    level = 0
    stack.clear()
    units.clear()
    lastState = Solver.UNKNOWN
    numberOfLearnedClauses = 0
  }

  /**
   * Mark the solver's initial stack position
   */
  def mark() {
    marks = constraints.length :: marks
  }

  /**
   * Undo all the additions until the last marked position.
   */
  def undo() {
    marks match {
      case h :: t =>
        marks = t
        lastState = Solver.UNKNOWN
        deleteConstraintsAndUpdateVariables(constraints.length - h)
      case _ =>
    }
  }

  /**
   *
   * @return the current model
   */
  def getModel = {
    require(lastState == Solver.SAT || lastState == Solver.UNSAT, "getModel(): Solver needs to be in SAT or UNSAT state!")

    lastState match {
      case Solver.UNSAT => None
      case Solver.SAT =>
        val partition = variables.values.partition(_.state == State.TRUE)
        val pos = partition._1.foldLeft(List[PLAtom]())((l, v) => l :+ PLAtom(v.name))
        val neg = partition._2.foldLeft(List[PLAtom]())((l, v) => l :+ PLAtom(v.name))
        Some(Model(pos, neg))
    }
  }

  /**
   * Solve the given set of constraints and set the state of the solver
   */
  private def solve() {
    while (true) {
      unitPropagation match {
        case Some(c) =>
          //conflict occurred => conflict has to be analyzed
          val solverState = analyzeConflictAndBacktrack(c)
          if (solverState == Solver.UNSAT) {
            lastState = Solver.UNSAT
            return
          }
        case None =>
          //no conflict occurred => assign a new chosen variable
          getUnassignedVar match {
            case None =>
              lastState = Solver.SAT
              return
            case Some(v) =>
              //increment level and assign the chosen variable
              level += 1
              v.assign(false, units, level, null)
              //update activity
              variables.values.map(_.activity *= 0.95)
              stack.push(v)
          }
      }
    }
  }

  /**
   * Print the variables and their value (TRUE, FALSE or OPEN)
   */
  def printVariables = {
    variables.values.toList.sortBy(_.ID).map { v =>
      println(v + ": " + v.state)
    }
  }

  /**
   * Unassign all variables
   */
  private def unassignVariables() {
    variables.values.map(_.unassign())
  }

  /**
   * Initialise all constraints.
   * Add all unit constraints to the set units and
   * set the solver state to unsat if an empty constraint is included
   */
  private def initConstraints() {
    constraints.map(c =>
      c.initWatchedLiterals match {
        case ConstraintState.UNIT => units += c;
        case ConstraintState.EMPTY => lastState = Solver.UNSAT;
        case _ =>
      }
    )
  }

  /**
   * Prepare the solver for the next solve call.
   */
  private def cleanUp() {
    level = 0
    stack.clear()
    units.clear()
    //delete all learned constraints
    if (numberOfLearnedClauses > 0) {
      deleteConstraints(numberOfLearnedClauses)
      numberOfLearnedClauses = 0
    }

    //reset the variables but don't change the state of the variable
    variables.values.map { v =>
      v.watched = ListBuffer[Constraint]()
      v.level = -1
      v.reason = null
      v.activity = 0.0
    }

    //reset the constraints
    constraints.map(
      _ match {
        case cardinality: PBLCardinalityConstraint =>
          cardinality.watchedLiterals = new ArrayBuffer[PBLTerm](cardinality.degree.+(1).toInt)
        case constraint: PBLConstraint =>
          constraint.currentSum = 0
          constraint.slack = 0
      }
    )
  }

  /**
   * Method computes the occurrences of the variables of the given set of constraints
   * @param constraints
   * @return the occurrences of the variables
   */
  private def computeOccurrences(constraints: List[Constraint]): mutable.HashMap[PBLVariable, Int] = {
    var occ = mutable.HashMap[PBLVariable, Int]()
    constraints.map(_.terms.map { t =>
      occ.get(t.l.v) match {
        case Some(o) => occ.update(t.l.v, o + 1)
        case None => occ.+=((t.l.v, 1))
      }
    })
    occ
  }

  /**
   * Treat all unit constraints and all literals which has to be propagated
   * @return Some(conflict) if a conflict occurs else None
   */
  private def unitPropagation: Option[Constraint] = {
    while (units.nonEmpty) {
      for (unit <- units) {
        val literals = unit.getLiteralsToPropagate
        //propagate all literals
        literals.map { l =>
          val conflict: Option[Constraint] =
            if (l.phase)
              l.v.assign(true, units, level, unit)
            else
              l.v.assign(false, units, level, unit)
          stack.push(l.v)
          conflict match {
            //return the conflict if one occurs
            case Some(c) => return Some(c)
            case _ =>
          }
        }
        //remove unit
        units.remove(unit)
      }
    }
    None
  }

  /**
   * Analyze the conflict and compute the level to backtrack.
   * Then backtrack to the computed level.
   * @param emptyClause the conflict
   * @return the new state of the solver (UNSAT or UNKNOWN)
   */
  private def analyzeConflictAndBacktrack(emptyClause: Constraint): Int = {
    var backtrackLevel = -1
    //conflict at level 0 => unsat
    if (level == 0) {
      Solver.UNSAT
    } else {
      //compute the constraint to learn
      var learnedConstraint = LearnUtil.learnPBConstraint(emptyClause, stack, level)
      //add the learned clause to the constraints
      constraints :+= learnedConstraint
      //update learned clause counter
      numberOfLearnedClauses += 1
      //update the units
      units = mutable.HashSet[Constraint](learnedConstraint)
      //update activity
      learnedConstraint.terms.map(_.l.v.activity += 1)

      /* compute backtracking level
       * backtracking level equals second largest level of the constraint
       */
      for (t <- learnedConstraint.terms) {
        val level: Int = t.l.v.level
        if (level != this.level && level > backtrackLevel) {
          backtrackLevel = level
        }
      }
      //check if learned clause is initial unit
      learnedConstraint match {
        case cardinality: PBLCardinalityConstraint =>
          if (BigInt(cardinality.terms.size) == cardinality.degree)
            backtrackLevel = 0
        case constraint: PBLConstraint =>
          val initialSlack = constraint.terms.map(_.a).sum - constraint.degree
          if (initialSlack >= 0 && constraint.terms.exists(_.a > initialSlack))
            backtrackLevel = 0
      }
      learnedConstraint = setWatchedLiterals(learnedConstraint, backtrackLevel)

      backtrack(backtrackLevel)

      /**
       * Only clauses will get unit after backtracking.
       * For other pb constraints you have to backtrack until the constraint is
       * unit or unresolved (success)
       */
      if (learnedConstraint.getCurrentState == ConstraintState.EMPTY) {
        if (backtrackLevel != 0) {
          backtrack(0)
        }
        else {
          return Solver.UNSAT
        }
      }
      Solver.UNKNOWN
    }
  }

  /**
   * Backtracking to the given level
   * @param level to backtrack
   */
  private def backtrack(level: Int) {
    while (stack.nonEmpty && stack.top.level > level) {
      val vari = stack.pop()
      vari.unassign()
    }
    //set the new level
    this.level = level
    /* Update all PBLConstraints
    * Not sure if this has to be done right here or could be done while computing the learned clause.
    * The problem is if you want to learn PBLConstraints you need the slack under the current assignment.
    * Updating the slack while computing the learned clause would lead to problems (in my opinion)
    */
    constraints.map { c =>
      if (c.isInstanceOf[PBLConstraint]) {
        //update slack and currentSum
        c.asInstanceOf[PBLConstraint].updateSlack()
        c.asInstanceOf[PBLConstraint].updateCurrentSum()
      }
    }
  }

  /**
   * Compute the next variable, which should be assigned
   * @return the next variable to assign
   */
  private def getUnassignedVar: Option[PBLVariable] = {
    // get all open variables
    val openVars = variables.values.filter(_.state == State.OPEN).toList
    if (openVars.isEmpty) {
      None
    } else {
      //find the variable with highest activity
      Some(openVars.maxBy(_.activity))
    }
  }

  /**
   * Method computes the watched literals of the new learned clause.
   * @param c the new learned clause
   * @param backtrackLevel the backtrack level
   * @return the constraint c after setting the watched literals
   */
  private def setWatchedLiterals(c: Constraint, backtrackLevel: Int): Constraint = {
    c match {
      case cardinality: PBLCardinalityConstraint =>
        val watched = new ArrayBuffer[PBLTerm](math.min(cardinality.terms.size, cardinality.degree.+(1).toInt))
        if (BigInt(cardinality.terms.size) == cardinality.degree) {
          //all literals have to be watched
          cardinality.terms.copyToBuffer(watched)
          watched.map(_.l.v.add(cardinality))
        } else {
          //set the literals which will be forced after backtracking
          for (t <- cardinality.terms) {
            if (t.l.v.level == level) {
              watched += t
              t.l.v.add(cardinality)
            }
          }
          //set the rest of the literals
          //case clause
          if (cardinality.degree == BigInt(1)) {
            val t = cardinality.terms.find(_.l.v.level == backtrackLevel).get
            watched += t
            t.l.v.add(cardinality)
            //case cardinality
          } else {
            //add those literals with highest level
            var terms = cardinality.terms.sortBy(_.l.v.level).toList.reverse
            while (watched.size != cardinality.degree.+(1).toInt) {
              if (!watched.contains(terms.head)) {
                watched += terms.head
                terms.head.l.v.add(cardinality)
              }
              terms = terms.tail
            }
          }
        }
        //set the computed watched literals and return the constraint
        cardinality.watchedLiterals = watched
        cardinality
      case c: PBLConstraint => c.initWatchedLiterals; c
    }
  }

  /**
   * Method deletes the last n constraints without updating the variables.
   * Variables which only occur at the deleted constraints aren't removed
   * @param n remove the last n constraints
   */
  private def deleteConstraints(n: Int) {
    val splittedConstraints = constraints.splitAt(constraints.size - n)
    //delete the last n constraints
    constraints = splittedConstraints._1
    //remove the constraints of watchedList of the variables
    splittedConstraints._2.map(c =>
      c.terms.map(t => t.l.v.watched -= c)
    )
  }

  /**
   * Method deletes the last n constraints with updating the variables.
   * Variables which only occur at the deleted constraints are removed
   * @param n remove the last n constraints
   */
  private def deleteConstraintsAndUpdateVariables(n: Int) {
    //compute all variables of deleted constraints
    val vars: List[PBLVariable] = constraints.splitAt(constraints.size - n)._2.map(_.terms.map(_.l.v)).flatten.distinct
    deleteConstraints(n)
    //compute all variables of the new constraints
    val varsOfConstraints: List[PBLVariable] = constraints.map(_.terms.map(_.l.v)).flatten.distinct
    //delete all variables which occur only at the deleted constraints
    (vars diff varsOfConstraints).map(variables -= _.ID)
  }
}

