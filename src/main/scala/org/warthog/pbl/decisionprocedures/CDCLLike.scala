package org.warthog.pbl.decisionprocedures

import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pbl.datastructures._
import org.warthog.pbl.parsers.PBCompetitionReader
import org.warthog.pl.decisionprocedures.satsolver.Solver
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
  var lastState = Solver.UNKNOWN
  var numberOfLearnedClauses = 0

  /**
   * Adding a constraint to the instance
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

  def solve(constraints: List[Constraint]): Int = {
    if (!constraints.isEmpty)
      add(constraints)
    //solve only if solver state is unknown
    if (lastState == Solver.UNKNOWN) {
      unassignVariables
      initConstraints
      solve
    }
    cleanUp
    lastState
  }

  /**
   * Main entry point for solving the given instance
   * @return true if the instance is sat else false
   */
  private def solve() {
    //else try to solve the instance
    while (true) {
      unitPropagation match {
        case Some(c) => {
          //conflict occurred => conflict has to be analyzed
          val backtrackLevel = analyzeConflict(c)
          if (backtrackLevel == -1) {
            lastState = Solver.UNSAT
            return
          }
          //backtracking to the computed level
          backtrack(backtrackLevel)
        }
        case None => {
          //non conflict occurred => assign a new chosen variable
          getUnassignedVar match {
            case None => {
              lastState = Solver.SAT
              return
            }
            case Some(v) => {
              level += 1
              v.assign(false, units, level, null)
              //update activity
              variables.values.map(_.activity *= 0.95)
              stack.push(v)
            }
          }
        }
      }
    }
  }

  def printVariables {
    variables.values.toList.sortBy(_.ID).map { v =>
      println(v.ID + ": " + v.state)
    }
  }

  private def unassignVariables {
    variables.values.map(_.unassign())
  }

  private def initConstraints() {
    constraints.map(c =>
      c.initWatchedLiterals match {
        case ConstraintState.UNIT => units += c;
        case ConstraintState.EMPTY => lastState = Solver.UNSAT;
        case _ =>
      }
    )
  }

  private def cleanUp {
    level = 0
    stack.clear()
    units.clear()
    //delete all learned constraints
    deleteConstraints(numberOfLearnedClauses)
    numberOfLearnedClauses = 0

    //reset the variables but don't change the state of the variable
    variables.values.map { v =>
      v.watched = ListBuffer[Constraint]()
      v.level = -1
      v.reason = null
      v.activity = 0.0
    }

    //reset the constraints
    constraints.map { c =>
      c match {
        case cardinality: PBLCardinalityConstraint => {
          cardinality.watchedLiterals = new ArrayBuffer[PBLTerm](cardinality.degree.+(1).toInt)
        }
        case constraint: PBLConstraint => {
          constraint.currentSum = 0
          constraint.slack = 0
        }
      }
    }
  }

  /**
   * Method computes the occurrences of the variables of the given instance
   * @param instance
   * @return the occurrences of the variables
   */
  private def computeOccurrences(instance: List[Constraint]): mutable.HashMap[PBLVariable, Int] = {
    var occ = mutable.HashMap[PBLVariable, Int]()
    instance.map(_.terms.map { t =>
      occ.get(t.l.v) match {
        case Some(o) => occ.update(t.l.v, o + 1)
        case None => {
          occ.+=((t.l.v, 1))
        }
      }
    })
    occ
  }

  /**
   * Treat all unit constraints and all literals which has to be propagated
   * @return
   */
  private def unitPropagation: Option[Constraint] = {
    while (!units.isEmpty) {
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
            case Some(c) => {
              return Some(c)
            }
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
   * Analyze the conflict and compute the level to backtrack
   * @param emptyClause the conflict
   * @return the backtrack level
   */
  private def analyzeConflict(emptyClause: Constraint): Int = {
    var backtrackLevel = -1
    if (level == 0)
      return -1
    //compute the clause to learn
    var learnedClause = LearnUtil.learnClause(emptyClause, stack, level)
    //update activity
    learnedClause.terms.map(_.l.v.activity += 1)
    //compute backtracking level
    for (t <- learnedClause.terms) {
      val level: Int = t.l.v.level
      if (level != this.level && level > backtrackLevel) {
        backtrackLevel = level
      }
    }
    learnedClause = setWatchedLiterals(learnedClause, backtrackLevel)
    //add the learned clause to the instance
    constraints :+= learnedClause
    //update learned clause counter
    numberOfLearnedClauses += 1
    //update the units
    units = mutable.HashSet[Constraint](learnedClause)
    //if learnedClause has only one literal
    if (learnedClause.terms.size == 1)
      return 0
    backtrackLevel
  }

  /**
   * Backtracking to the given level
   * @param level to backtrack
   */
  private def backtrack(level: Int) {
    while (!stack.isEmpty && stack.top.level > level) {
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
        c.asInstanceOf[PBLConstraint].updateSlack
        c.asInstanceOf[PBLConstraint].updateCurrentSum
      }
    }
  }

  /**
   * Compute the next variable, which should be assigned
   * The more a variable occurs the earlier it is chosen
   * @return the next variable to assign
   */
  private def getUnassignedVar: Option[PBLVariable] = {
    // get all open variables
    val openVars = variables.values.filter(_.state == State.OPEN).toList
    if (openVars.isEmpty) {
      None
    } else {
      //find the variable with highest occurrence
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
      case cardinality: PBLCardinalityConstraint => {
        val watched = new ArrayBuffer[PBLTerm](math.min(cardinality.terms.size, (cardinality.degree.+(1).toInt)))
        if (cardinality.terms.size == cardinality.degree) {
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
          for (t <- cardinality.terms) {
            if (watched.size != cardinality.degree.+(1) && !watched.contains(t) && t.l.v.level == backtrackLevel) {
              watched += t
              t.l.v.add(cardinality)
            }
          }
        }
        cardinality.watchedLiterals = watched
        cardinality
      }
      //TODO treat pseudo-boolean constraints
      case _ => c
    }
  }

  private def deleteConstraints(n: Int) {
    val splittedConstraints = constraints.splitAt(constraints.size - n)
    constraints = splittedConstraints._1
    //remove the learned constraints of watchedList of the variables
    splittedConstraints._2.map(c =>
      c.terms.map(t => t.l.v.watched -= c)
    )
  }
}

