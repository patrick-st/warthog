package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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
  var occurrences =  mutable.HashMap[PBLVariable, Int]()

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
    this.occurrences = this.computeOccurrences(instance)
  }


  private def computeOccurrences(instance: List[Constraint]): mutable.HashMap[PBLVariable, Int] = {
    var occ = mutable.HashMap[PBLVariable, Int]()
    instance.map(_.terms.map{t =>
     occ.get(t.l.v) match {
       case Some(o) => occ.update(t.l.v,o + 1)
       case None => {
         occ.+=((t.l.v, 1))
       }
     }
    })
    occ
  }


  def solve : Boolean = {
    while(true){
      this.unitPropagation match {
        case Some(c) => {
          val backtrackLevel = this.analyzeConflict(c)
          if(backtrackLevel == -1)
            return false
          this.backtrack(backtrackLevel)
        }
        case None => {
          this.getUnassignedVar match {
            case None => return true
            case Some(v) => {
              this.level += 1
              v.assign(false,units,level,null)
              this.stack.push(v)
            }
          }
        }
      }
    }
    false
  }


  /**
   * Treat all unit constraints and all literals which has to be propagated
   * @return
   */
   def unitPropagation: Option[Constraint] ={
    while(!units.isEmpty){
      for(unit <- units){
        val literals = unit.getLiteralsToPropagate
        literals.map{l =>
          val conflict: Option[Constraint] =
            if(l.phase)
              l.v.assign(true, units, level, unit)
            else
              l.v.assign(false, units, level, unit)
          stack.push(l.v)
          conflict match {
            case Some(c) => {
              return Some(c)
            }
            case _ =>
          }
        }
        //remove unit
        units = units.filter(!_.equals(unit))
      }
    }
    None
  }


  def analyzeConflict(emptyClause: Constraint): Int = {
    var backtrackLevel = -1
    if(this.level == 0)
      return -1
    //compute the clause to learn
    var learnedClause = LearnUtil.learnClause(emptyClause,stack,level)
    //compute backtracking level
    for (t <- learnedClause.terms) {
      val level: Int = t.l.v.level
      if (level != this.level && level > backtrackLevel) {
        backtrackLevel = level
      }
    }
    learnedClause = this.setWatchedLiterals(learnedClause, this.level, backtrackLevel)
    instance +:= learnedClause
    //update the units
    units = ListBuffer[Constraint](learnedClause)

    //if learnedClause has only one literal
    if(learnedClause.terms.size == 1)
      return 0

    backtrackLevel
  }


  def backtrack(level: Int): Unit ={
    while(!stack.isEmpty && stack.top.level > level){
      val vari = stack.pop()
      vari.unassign()
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


  private def getUnassignedVar: Option[PBLVariable] = {
    var unassigned: PBLVariable = null
    val openVars = variables.values.filter(_.state == State.OPEN).toList
    if(openVars.isEmpty){
      None
    } else {
     Some(openVars.maxBy(this.occurrences.get(_).get))
    }
  }
  

  private def setWatchedLiterals(c: Constraint, level: Int, backtrackLevel: Int): Constraint = {
    if(c.isInstanceOf[PBLCardinalityConstraint]) {
      val cardinality = c.asInstanceOf[PBLCardinalityConstraint]
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
          if(watched.size != cardinality.degree.+(1) && !watched.contains(t) && t.l.v.level == backtrackLevel){
            watched += t
            t.l.v.add(cardinality)
          }
        }
      }
      cardinality.watchedLiterals = watched
      cardinality

    } else
      c
  }
}


