package org.warthog.pbl.decisionprocedures

import org.warthog.pbl.datastructures._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 *
 */
object LearnUtil {

  def learnClause(conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int) =
    this.learn(reduce2Clause,reduce2Clause,resolve,conflict,stack,level)

 private def learn(reduce1: (Constraint, PBLVariable) => Constraint, reduce2: (Constraint, PBLVariable) => Constraint,
             resolve: (Constraint,Constraint,PBLVariable) => Constraint,
             conflict: Constraint, stack: mutable.Stack[PBLVariable], level: Int): Constraint = {
   for(v <- stack.iterator){
   }

   var c1 = conflict
    while(!stack.isEmpty){
      val v = stack.pop()
      c1 = reduce1(c1,v)
      v.unassign()
      val c2 = reduce2(v.reason, v)
      c1 = this.resolve(c1, c2, v)
      if(this.is1UIP(c1, level)){
        return c1
      }
    }
    null
  }


  /**
   * Reduce the given constraint to a clause
   * @param c the constraint to reduce
   * @param v this variable don't have to be removed
   *          because over this variable will be resolved
   * @return the reduced clause
   */
   private def reduce2Clause(c: Constraint, v: PBLVariable): Constraint = {
    if (c.isInstanceOf[PBLCardinalityConstraint] && c.asInstanceOf[PBLCardinalityConstraint].degree == 1) {
      c.copy
    } else {
      val newTerms = ListBuffer[PBLTerm]()
      c.terms.foldLeft(newTerms) { (buffer, term) =>
        if (term.l.evaluates2False || term.l.v == v) {
          buffer += new PBLTerm(1, term.l.copy)
        }
        buffer
      }
      new PBLCardinalityConstraint(newTerms.toList, 1)
    }
  }



  /**
   * Checks if the constraints are resolvable or not
   * @param c1 first constraint
   * @param c2 second constraint
   * @return true if c1 and c2 are resolvable else false
   */
   private def isResolvable(c1: Constraint, c2: Constraint): Boolean ={
    for(t1 <- c1.terms){
      if(c2.terms.exists{ t2 =>
        t2.l.v == t1.l.v && t2.l.phase != t1.l.phase
      }) return true
    }
    false
  }

  /**
   * Computes the resolvent of two given clauses
   * @param c1 first clause
   * @param c2 second clause
   * @param v the variable to resolve
   * @return the resolvent
   */
  private def resolve(c1: Constraint, c2: Constraint, v: PBLVariable) = {
    if(this.isResolvable(c1,c2)){
      val newTerms = (c1.terms.filter(_.l.v != v) union c2.terms.filter(_.l.v != v)).distinct.foldLeft(List[PBLTerm]())(_ :+ _.copy )
      new PBLCardinalityConstraint(newTerms, 1)
    } else {
      c1
    }
  }

  private def is1UIP(c1: Constraint, level: Int) ={
    c1.terms.filter(_.l.v.level == level).size == 1
  }

}

