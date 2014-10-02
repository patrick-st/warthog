package org.warthog.pbl.decisionprocedures

import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pbl.datastructures._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Implements a CDCL like approach to solve pseudo-boolean constraints
 */
class CDCLLike {

  var instance = List[Constraint]()
  val objectiveFunction = List[PBLTerm]()
  var level = 0
  val stack = mutable.Stack[PBLVariable]()
  var variables = mutable.HashMap[Int,PBLVariable]()
  var units = ListBuffer[Constraint]()

  def this(instance: List[Constraint], variables: mutable.HashMap[Int, PBLVariable]){
    this()
    this.instance = instance.map{c =>
      c.initWatchedLiterals match {
        case ConstraintState.UNIT => this.units :+= c
        case _ =>
      }
      c
    }
    this.variables = variables
  }

  def solve : Boolean = {
    while(true){
      //println("units: " + units.size)
      this.unitPropagation match {
        case Some(c) => {
          //println("conflict: " + c)
          val backtrackLevel = this.analyzeConflict(c)
          //println("backtracklevel: " + backtrackLevel)
          if(backtrackLevel == -1)
            return false
          this.backtrack(backtrackLevel)
        }
        case None => {
          this.getUnassignedVar match {
            case None => return true
            case Some(v) => {
              this.level += 1
              //println("assigned: " + v + " to false")
              v.assign(false,units,level,null)
              this.stack.push(v)
            }
          }
        }
      }
    }
    false
  }

  def printVariables = {
    variables.values.map{v =>
      println(v.name + ": " + v.state)
    }
  }

  /**
   * Treat all unit constraints and all literals which has to be propagated
   * @return
   */
  private def unitPropagation: Option[Constraint] ={
    while(!units.isEmpty){
      for(unit <- units){
        val literals = unit.getLiteralsToPropagate
       // println("literals to propagate: " + literals)
        literals.map{l =>
          val conflict: Option[Constraint] =  if(l.phase) {
            //println("propagated: " + l.v + " to true")
            l.v.assign(true, units, level, unit)
          }
            else {
           // println("propagated: " + l.v + " to false")
            l.v.assign(false, units, level, unit)
          }
          stack.push(l.v)
          //println("conflict: " + conflict)
          conflict match {
            case Some(c) => return Some(c)
            case _ =>
          }
        }
        //remove unit
        units = units.filter(!_.equals(unit))
      }
    }
    None
  }


  private def analyzeConflict(emptyClause: Constraint): Int = {
    if(this.level == 0)
      return -1
    //compute the clause to learn
    val learnedClause = LearnUtil.learnClause(emptyClause,stack,level)
   // println("learned: " + learnedClause)
    //compute backtracking level
    val backtrackingLevel: Int = learnedClause.terms.minBy(_.l.v.level).l.v.level
    //set the watched literals of learnedClause
    learnedClause.initWatchedLiterals
    //println("watched of learend clause: " + learnedClause.asInstanceOf[PBLCardinalityConstraint].watchedLiterals)
    instance +:= learnedClause

    //update the units
    units = ListBuffer[Constraint](learnedClause)

    //if learnedClause has only one literal
    if(learnedClause.terms.size == 1)
      return 0

    backtrackingLevel
  }


  private def backtrack(level: Int): Unit ={
    while(!stack.isEmpty && stack.top.level > level){
     // println(" bevore backtracking: " + stack.top + ", " + stack.top.level, stack.top.state)
      val vari = stack.pop()
      vari.unassign()
      //println(" after backtracking: " + vari + ", " + vari.level, vari.state)
    }
    //set the new level
    this.level = level
    //update all PBLConstraints
    instance.map{c =>
      if(c.isInstanceOf[PBLConstraint]){
        //update slack and currentSum
        c.asInstanceOf[PBLConstraint].updateSlack
        c.asInstanceOf[PBLConstraint].updateCurrentSum
      }
    }
  }

  /**
   * Searches for an unassigned variable to assign
   * @return an unassigned variable
   */
  private def getUnassignedVar = {
    var unassigned: PBLVariable = null
    if(variables.values.exists{v =>  unassigned = v; v.state == State.OPEN}){
      Some(unassigned)
    } else {
      None
    }
  }
}

object Main{
  def main(args: Array[String]) {
    val instance = DIMACSReader.dimacs2PBConstraints("src\\test\\resources\\dimacs\\uf150-010.cnf")
    val solver = new CDCLLike(instance._1, instance._2)
    println("start to solve")
    println(solver.solve)
    solver.printVariables
  }
}
