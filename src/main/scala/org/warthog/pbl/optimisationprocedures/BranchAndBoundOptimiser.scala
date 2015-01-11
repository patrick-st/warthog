package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures._
import org.warthog.pl.decisionprocedures.satsolver.Model
import org.warthog.pl.formulas.PLAtom

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

/**
 * Implementation of a branch and bound optimisation approach
 */
class BranchAndBoundOptimiser extends OptimisationProcedure {

  var constraints: List[Constraint] = List[Constraint]()
  var minOptimum: BigInt = null
  var objectiveFunction: List[PBLTerm] = null
  var normalizedFunction: List[PBLTerm] = null
  var level = 0
  var stack = mutable.Stack[PBLVariable]()
  var variables = mutable.HashMap[Int, PBLVariable]()
  var units = mutable.HashSet[Constraint]()
  var containsEmptyConstraint = false
  var model: Option[Model] = None

  def add(c: Constraint) {
    //exchange variables if necessary
    c.terms.map { t =>
      t.l.v = variables.getOrElseUpdate(t.l.v.ID, t.l.v)
    }

    constraints :+= c
    minOptimum = null
  }

  def reset() {
    constraints = List[Constraint]()
    minOptimum = null
    objectiveFunction = null
    normalizedFunction = null
    level = 0
    stack.clear()
    variables.clear()
    units.clear()
    containsEmptyConstraint = false
    model = None
  }

  def min(objectiveFunction: List[PBLTerm]): Option[BigInt] = {
    //check if optimum was already computed
    if (minOptimum == null) {

      unassignVariables
      initConstraints()

      //exchange variables of objective function
      objectiveFunction.map { t =>
        t.l.v = variables.getOrElseUpdate(t.l.v.ID, t.l.v)
      }

      //check if the instance contains an empty constraint
      if (this.containsEmptyConstraint)
        return None
      else {
        //set the minimization function
        this.objectiveFunction = objectiveFunction
        //normalize the objective function
        normalizedFunction = objectiveFunction.foldLeft(List[PBLTerm]())(_ :+ _.copy).map { t =>
          if (t.a < 0) {
            t.a = t.a.abs
            t.l = t.l.negate
          }
          t
        }

        //set initial upper bound to the maximum of the objective function
        val ub = normalizedFunction.filter(_.a > 0).map(_.a).sum + 1
        solve(ub)
      }
      cleanUp()
    }
    //check if an optimum was found
    if (minOptimum == null)
      None
    else
      Some(minOptimum)
  }

  def getModel = model

  private def solve(currentUB: BigInt): Option[BigInt] = {
    unitPropagation match {
      //the instance can't be satisfied with this partial assignment
      case Some(c) =>
        //update activity
        c.terms.map(_.l.v.activity += 1)
        Some(currentUB)
      case None =>
        //lb = current value of objective function
        val lb = normalizedFunction.filter(_.l.evaluates2True).map(_.a).sum + maximalIndependentSet

        if (lb >= currentUB) {
          return Some(currentUB)
        }
        //chose next variable
        getNextVar match {
          //new optimum found
          case None =>
            minOptimum = objectiveFunction.filter(_.l.evaluates2True).map(_.a).sum
            model = computeModel()
            Some(normalizedFunction.filter(_.l.evaluates2True).map(_.a).sum)
          case Some(v) =>
            //update activity
            variables.values.map(_.activity *= 0.95)
            val backtrackLevel = level

            level += 1
            //first assign the variable to false
            v.assign(false, units, level, null)
            stack.push(v)
            val newUB = currentUB.min(solve(currentUB).get)
            backtrack(backtrackLevel)
            level += 1
            //now assign the variable to true
            v.assign(true, units, level, null)
            stack.push(v)
            Some(newUB.min(solve(newUB).get))
        }
    }
  }

  private def cleanUp() {
    level = 0
    stack.clear()
    units.clear()

    //reset the variables but don't change the state of the variable
    variables.values.map { v =>
      v.watched = ListBuffer[Constraint]()
      v.level = -1
      v.reason = null
      v.activity = 0.0
    }

    //reset the constraints
    constraints.map {
      case cardinality: PBLCardinalityConstraint =>
        cardinality.watchedLiterals = new ArrayBuffer[PBLTerm](cardinality.degree.+(1).toInt)
      case constraint: PBLConstraint =>
        constraint.currentSum = 0
        constraint.slack = 0
    }
  }

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
            case Some(c) =>
              return Some(c)
            case _ =>
          }
        }
        //remove unit
        units.remove(unit)
      }
    }
    None
  }

  private def getNextVar: Option[PBLVariable] = {
    // get all open variables
    val openVars = variables.values.filter(_.state == State.OPEN).toList
    if (openVars.isEmpty) {
      None
    } else {
      Some(openVars.maxBy(_.activity))
    }
  }

  private def backtrack(level: Int): Unit = {
    units = mutable.HashSet[Constraint]()
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

  private def maximalIndependentSet: BigInt = {
    var independentSet = Set[PBLVariable]()
    //collect all variables of the objective function
    val varsObjective = objectiveFunction.foldLeft(Set[PBLVariable]())(_ + _.l.v)
    var cost: BigInt = 0
    constraints.foreach { c =>
      //check if constraint is already sat
      val isSat = c.terms.filter(_.l.evaluates2True).map(_.a).sum >= c.degree
      if (!isSat) {
        val varsC = c.terms.foldLeft(Set[PBLVariable]())(_ + _.l.v)
        if ((independentSet intersect varsC).isEmpty) {
          independentSet ++= c.terms.foldLeft(independentSet)(_ + _.l.v)
          cost += minimalCost(c.copy, varsObjective)
        }
      }
    }
    cost
  }

  private def minimalCost(c: Constraint, varsObjective: Set[PBLVariable]): BigInt = {
    var terms = normalizedFunction.foldLeft(mutable.HashMap[Int, BigInt]()) { (map, t) => map += t.l.v.ID -> t.a}
    //determine degree of corresponding cardinality constraint
    var degree_ = c.degree
    //collect all variables who evaluate to true and don't cause any costs
    val noCostVariables = c.terms.filter { t => t.l.evaluates2True || (t.l.v.state == State.OPEN && !varsObjective.contains(t.l.v))}
    degree_ -= noCostVariables.map(_.a).sum
    //sorted list of the cost variables from min to max
    var minCostVariables = c.terms.filter(_.l.v.state == State.OPEN).diff(noCostVariables).sortBy { t => terms(t.l.v.ID)}
    //sorted list of the cost variables from max to min
    var maxKoeffVariables = c.terms.filter(_.l.v.state == State.OPEN).diff(noCostVariables).sortBy(_.a).reverse
    //k is the degree of the cardinality constraint
    var k = 0
    var s: BigInt = 0
    while (s < degree_ && maxKoeffVariables.nonEmpty) {
      s += maxKoeffVariables(0).a
      maxKoeffVariables = maxKoeffVariables.tail
      k += 1
    }
    var cost: BigInt = 0
    for (i <- 0 to k - 1) {
      cost += terms(minCostVariables(i).l.v.ID)
    }
    cost
  }

  private def initConstraints() {
    constraints.map(c =>
      c.initWatchedLiterals match {
        case ConstraintState.UNIT => units += c;
        case ConstraintState.EMPTY => containsEmptyConstraint = true
        case _ =>
      }
    )
  }

  private def unassignVariables() {
    variables.values.map(_.unassign())
  }

  private def computeModel() = {
    val partition = variables.values.partition(_.state == State.TRUE)
    val pos = partition._1.foldLeft(List[PLAtom]())((l, v) => l :+ PLAtom(v.name))
    val neg = partition._2.foldLeft(List[PLAtom]())((l, v) => l :+ PLAtom(v.name))
    Some(Model(pos, neg))
  }
}
