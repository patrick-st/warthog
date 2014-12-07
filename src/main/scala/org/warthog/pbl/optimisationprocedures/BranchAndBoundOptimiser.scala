package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures._

import scala.collection.mutable

/**
 * Implementation of a branch and bound optimisation approach
 */
class BranchAndBoundOptimiser extends Optimisationprocedure {

  var instance: List[Constraint] = List[Constraint]()
  var optimum: BigInt = null
  var objectiveFunction: List[PBLTerm] = null
  var normalizedFunction: List[PBLTerm] = null
  var level = 0
  var stack = mutable.Stack[PBLVariable]()
  var variables = mutable.HashMap[Int, PBLVariable]()
  var units = mutable.HashSet[Constraint]()
  var containsEmptyConstraint = false

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

  def add(constraintList: List[Constraint]) = constraintList.map(this.add(_))

  def reset(): Unit = {
    this.backtrack(0)
    this.optimum = null
    this.objectiveFunction = null
    this.normalizedFunction = null
  }

  def solve(objectiveFunction: List[PBLTerm]): Option[BigInt] = {
    //exchange variables of objective function
    objectiveFunction.map{t =>
      t.l.v = this.variables.getOrElseUpdate(t.l.v.ID,t.l.v)
    }

    //check if the instance contains an empty constraint
    if(this.containsEmptyConstraint)
      return None

    //set the minimization function
    this.objectiveFunction = objectiveFunction
    //normalize the objective function
    this.normalizedFunction = this.objectiveFunction.foldLeft(List[PBLTerm]())(_ :+ _.copy).map{t =>
      if(t.a < 0){
        t.a = t.a.abs
        t.l = t.l.negate
      }
      t
    }

    //set initial upper bound to the maximum of the objective function
    val ub = this.normalizedFunction.filter(_.a > 0).map(_.a).sum + 1
    this.solve(ub)
    //check if an optimum was found
    if(this.optimum == null)
       None
    else
      Some(this.optimum)
  }


  private def solve(currentUB: BigInt): Option[BigInt] = {
    this.unitPropagation match {
      //the instance can't be satisfied with this partial assignment
      case Some(c) => {
        //update activity
        c.terms.map(_.l.v.activity += 1)
        return Some(currentUB)
      }
      case None => {
        //lb = current value of objective function
        val lb = this.normalizedFunction.filter(_.l.evaluates2True).map(_.a).sum + this.maximalIndependentSet

        if(lb >= currentUB) {
          return Some(currentUB)
        }
        //chose next variable
        this.getNextVar match {
          case None => {
            this.optimum = this.objectiveFunction.filter(_.l.evaluates2True).map(_.a).sum
            return Some(this.normalizedFunction.filter(_.l.evaluates2True).map(_.a).sum)
          }
          case Some(v) => {
            //update activity
            this.variables.values.map(_.activity *= 0.95)
            val backtrackLevel = this.level

            this.level += 1
            //first assign the variable to false
            v.assign(false,units,level,null)
            this.stack.push(v)
            val newUB = currentUB.min(this.solve(currentUB).get)
            this.backtrack(backtrackLevel)
            this.level += 1
            //now assign the variable to true
            v.assign(true,units,level,null)
            this.stack.push(v)
            return Some(newUB.min(this.solve(newUB).get))
          }
        }
      }
    }


  }


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
    this.units = mutable.HashSet[Constraint]()
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

  def maximalIndependentSet : BigInt = {
    var independentSet = Set[PBLVariable]()
    //collect all variables of the objective function
    val varsObjective = this.objectiveFunction.foldLeft(Set[PBLVariable]())(_ + _.l.v)
    var cost: BigInt = 0
    this.instance.foreach{c =>
      //check if constraint is already sat
      val isSat = c.terms.filter(_.l.evaluates2True).map(_.a).sum >= c.degree
      if(!isSat){
        val varsC = c.terms.foldLeft(Set[PBLVariable]())(_ + _.l.v)
        if((independentSet intersect varsC).isEmpty){
          independentSet ++= c.terms.foldLeft(independentSet)(_ + _.l.v)
          cost += this.minimalCost(c.copy, varsObjective)
        }
      }
    }
    cost
  }

  def minimalCost(c: Constraint, varsObjective: Set[PBLVariable]): BigInt = {
    var terms = this.normalizedFunction.foldLeft(mutable.HashMap[Int, BigInt]()){(map,t) => map += t.l.v.ID -> t.a}
    //determine degree of corresponding cardinality constraint
    var degree_ = c.degree
    //collect all variables who evaluate to true and don't cause any costs
    val noCostVariables = c.terms.filter{t => t.l.evaluates2True || (t.l.v.state == State.OPEN && !varsObjective.contains(t.l.v))}
    degree_ -= noCostVariables.map(_.a).sum
    //sorted list of the cost variables from min to max
    var minCostVariables = c.terms.filter(_.l.v.state == State.OPEN).diff(noCostVariables).sortBy{t => terms(t.l.v.ID)}
    //sorted list of the cost variables from max to min
    var maxKoeffVariables = c.terms.filter(_.l.v.state == State.OPEN).diff(noCostVariables).sortBy(_.a).reverse
    //k is the degree of the cardinality constraint
    var k = 0
    var s: BigInt = 0
    while(s < degree_ && !maxKoeffVariables.isEmpty){
      s += maxKoeffVariables(0).a
      maxKoeffVariables = maxKoeffVariables.tail
      k += 1
    }
    var cost: BigInt = 0
    for(i <- 0 to k-1){
      cost += terms(minCostVariables(i).l.v.ID)
    }
    cost
  }


}
