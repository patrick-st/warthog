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
        return Some(currentUB)
      }
      case None => {
        //lb = current value of objective function
        val lb = this.normalizedFunction.filter(_.l.evaluates2True).map(_.a).sum

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
      Some(openVars(0))
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


}
