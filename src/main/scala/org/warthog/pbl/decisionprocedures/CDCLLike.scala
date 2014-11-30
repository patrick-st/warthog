package org.warthog.pbl.decisionprocedures

import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pbl.datastructures._
import org.warthog.pbl.parsers.PBCompetitionReader
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Implements a CDCL like approach to solve pseudo-boolean constraints
 */
class CDCLLike extends Decisionprocedure {

  var instance = List[Constraint]()
  var level = 0
  var stack = mutable.Stack[PBLVariable]()
  var variables = mutable.HashMap[Int, PBLVariable]()
  var units = mutable.HashSet[Constraint]()
  var containsEmptyConstraint = false


  /**
   * Adding a constraint to the instance
   * @param c the constraint to add
   */
  def add(c: Constraint) = {
    //exchange variables if necessary
    c.terms.map{t =>
     t.l.v = this.variables.getOrElseUpdate(t.l.v.ID,t.l.v)
    }

    c.initWatchedLiterals match {
      case ConstraintState.UNIT => this.units += c; this.instance ::= c
      case ConstraintState.EMPTY => this.containsEmptyConstraint = true; this.instance ::=c
      case _ => this.instance ::= c
    }
  }

  /**
   * Adding a list of constraints to the instance
   * @param constraintList the list to add
   */
  def add(constraintList: List[Constraint]) = {
    constraintList.map(this.add(_))
  }


  /**
   * Main entry point for solving the given instance
   * @return true if the instance is sat else false
   */
  def solve(constraints: List[Constraint]): Boolean = {
    //enforce that all constraints are removable
    constraints.map(_.removable = true)
    //add the constraints to the instance
    this.add(constraints)
    //check if the instance contains an empty constraint
    if(this.containsEmptyConstraint)
      return false

    //else try to solve the instance
    while (true) {
      this.unitPropagation match {
        case Some(c) => {
          //conflict occurred => conflict has to be analyzed
          val backtrackLevel = this.analyzeConflict(c)
          if (backtrackLevel == -1)
            return false
          //backtracking to the computed level
          this.backtrack(backtrackLevel)
        }
        case None => {
          //non conflict occurred => assign a new chosen variable
          this.getUnassignedVar match {
            case None => return true
            case Some(v) => {
              this.level += 1
              v.assign(false, units, level, null)
              //update activity
              this.variables.values.map(_.activity *= 0.95)
              this.stack.push(v)
            }
          }
        }
      }
    }
    false
  }


  def printVariables = {
    this.variables.values.toList.sortBy(_.ID).map{v =>
    println(v.ID + ": " + v.state)
    }
}

  private def initConstraints = {
    this.instance.map{c =>
          c.initWatchedLiterals match {
            case ConstraintState.UNIT => this.units += c;
            case ConstraintState.EMPTY => this.containsEmptyConstraint = true;
            //ignore the sat constraints
            case _  =>
          }
        }
  }

  /**
   * Method resets the instance to the initial state
   */
  def reset(): Unit = {
    this.level = 0
    this.stack = new mutable.Stack[PBLVariable]()
    this.containsEmptyConstraint = false

    //delete all learned Constraints
    this.instance = this.instance.filterNot(_.removable)

    //reset all variables
    this.variables = mutable.HashMap[Int, PBLVariable]()
    this.instance.map{c =>
      c.terms.map{t =>
        val v = t.l.v
        v.state = State.OPEN
        v.watched = ListBuffer[Constraint]()
        v.level = -1
        v.reason = null
        v.activity = 0.0
        this.variables.update(t.l.v.ID,v)
      }
    }

    this.units =  mutable.HashSet[Constraint]()
    //reset all constraints and init the watched literals
    this.instance.map{c =>
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

    this.initConstraints
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
    if (this.level == 0)
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
    learnedClause = this.setWatchedLiterals(learnedClause, backtrackLevel)
    //add the learned clause to the instance
    instance +:= learnedClause
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
  private def backtrack(level: Int): Unit = {
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
    instance.map { c =>
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
}

