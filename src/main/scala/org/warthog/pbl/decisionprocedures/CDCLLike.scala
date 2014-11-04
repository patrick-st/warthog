package org.warthog.pbl.decisionprocedures

import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pbl.datastructures._
import org.warthog.pbl.parsers.PBCompetitionReader
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Implements a CDCL like approach to solve pseudo-boolean constraints
 */
class CDCLLike {

  var instance = List[Constraint]()
  var objectiveFunction: Constraint = null
  var optimum: BigInt = null
  var level = 0
  var stack = mutable.Stack[PBLVariable]()
  var variables = mutable.HashMap[Int, PBLVariable]()
  var units = mutable.HashSet[Constraint]()
  var occurrences = mutable.HashMap[PBLVariable, Int]()

  def this(instance: List[Constraint], objective: Option[List[PBLTerm]], variables: mutable.HashMap[Int, PBLVariable]) {
    this()
    this.variables = variables
    this.instance = instance
    // add the objectiveFunction
    if(objective != None) {
      val objectiveFunction = objective.get
      //compute the right-hand side of the objective function
      val rhs: BigInt = objectiveFunction.filter(_.a > 0).map(_.a).sum
      objectiveFunction.map(_.a *= -1)
      //check if objective function is cardinality
      if (objectiveFunction.forall(_.a.abs == objectiveFunction(0).a.abs)) {
        this.objectiveFunction = new PBLCardinalityConstraint(objectiveFunction, -rhs)
      } else {
        this.objectiveFunction = new PBLConstraint(objectiveFunction, -rhs)
      }
    }
    //init the watched literals
    this.instance.map { c =>
      c.initWatchedLiterals match {
        case ConstraintState.UNIT => this.units += c
        case _ =>
      }
      c
    }
  }


  /**
   * Main entry point for optimizing the given instance by binary search
   */
  def binarySearchOptimisation() = {
    //determine upper and lower bound for binary search
    var upper = this.objectiveFunction.terms.map(_.a).sum
    var lower = this.objectiveFunction.degree
    while(lower <= upper){
      val middle = (lower + upper) / 2
      //enforce  objectiveFunction >= middle
      var upperConstraint = this.objectiveFunction.copy
      upperConstraint.degree = middle
      upperConstraint.initWatchedLiterals match {
        case ConstraintState.UNIT => this.units += upperConstraint
        case _ =>
      }
      //enforce objectiveFunction <= upper
      var lowerConstraint = this.objectiveFunction.copy
      lowerConstraint.degree = upper
      //normalize the lowerConstraint
      lowerConstraint * -1
      lowerConstraint.normalize()
      lowerConstraint.initWatchedLiterals match {
        case ConstraintState.UNIT => this.units += lowerConstraint
        case _ =>
      }
      //add the constraints to the instance
      this.instance +:= upperConstraint
      this.instance +:= lowerConstraint
      if(this.solve){
        val opt = this.objectiveFunction.terms.filter(_.l.evaluates2True).map(_.a).sum
        this.optimum = opt
        lower = middle + 1
      } else {
        upper = this.optimum-1
      }
      this.reset()
    }
  }


  /**
   * Main entry point for optimizing the given instance by linear search
   */
  def linearSearchOptimisation() = {
    //add the objectiveFunction to the instance
    val objective = this.objectiveFunction.copy
    objective.initWatchedLiterals match {
      case ConstraintState.UNIT => this.units += objective
      case _ =>
    }
    this.instance +:= objective
    //start to optimize
    while(this.solve){
      //compute the current optimum
      val opt = this.objectiveFunction.terms.filter(_.l.evaluates2True).map(_.a).sum
      //update the objective function
      objective.degree = opt + 1
      this.optimum = opt
      this.reset()
    }
  }

  /**
   * Main entry point for solving the given instance
   * @return true if the instance is sat else false
   */
  def solve: Boolean = {
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

  /**
   * Method resets the instance to the initial state
   */
  private def reset(): Unit = {
    this.level = 0
    this.stack = new mutable.Stack[PBLVariable]()
    //reset all variables
    this.variables.values.map{v =>
      v.unassign()
      v.reason = null
      v.activity = 0
      v.watched = new ListBuffer[Constraint]()
    }
    //delete all learned Constraints
    this.instance = this.instance.filterNot(_.learned)

    this.units =  mutable.HashSet[Constraint]()
    //reset all constraints and init the watched literals
    this.instance.map{c =>
      c match {
        case cardinality: PBLCardinalityConstraint => {
          cardinality.watchedLiterals = new ArrayBuffer[PBLTerm](cardinality.degree.+(1).toInt)
          cardinality.initWatchedLiterals() match {
            case ConstraintState.UNIT => this.units += cardinality
            case _ =>
          }
        }
        case constraint: PBLConstraint => {
          constraint.currentSum = 0
          constraint.slack = 0
          constraint.initWatchedLiterals() match {
            case ConstraintState.UNIT => this.units += constraint
            case _ =>
          }
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
