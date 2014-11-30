package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures.{PBLConstraint, PBLCardinalityConstraint, Constraint, PBLTerm}
import org.warthog.pbl.decisionprocedures.{Decisionprocedure, CDCLLike}

/**
 * Implementation of an optimiser, which uses linear search to compute the optimum
 */
class LinearOptimiser extends Optimisationprocedure{
  val solver : Decisionprocedure = new CDCLLike()
  //the original parsed minimisation function of the opb-file
  var minimizeFunction: List[PBLTerm] = null
  //the computed maximisation function out of the minimisation function
  var maximizeFunction: Constraint = null
  // the optimum computed by the minimisation function
  var minOptimum: BigInt = null
  // the optimum computed by the maximisation function
  var maxOptimum: BigInt = null


  def add(c: Constraint) = this.solver.add(c)

  def add(constraints: List[Constraint]) = this.solver.add(constraints)

  def reset() = {
    this.solver.reset()
    this.minimizeFunction = null
    this.maximizeFunction = null
    this.minOptimum = null
    this.maxOptimum = null
  }

  /**
   * Main entry point for optimizing the given instance by linear search
   */
  def solve(objectiveFunction: List[PBLTerm]): Option[BigInt] = {
    this.minimizeFunction = objectiveFunction.foldLeft(List[PBLTerm]())(_ :+ _.copy)
    //compute the maximization function
    val rhs: BigInt = objectiveFunction.filter(_.a > 0).map(_.a).sum
    //multiply with -1 to get the maximization function
    objectiveFunction.map(_.a *= -1)
    //check if objective function is cardinality
    if (objectiveFunction.forall(_.a.abs == objectiveFunction(0).a.abs)) {
      //Note: set the removable flag to true
      this.maximizeFunction = new PBLCardinalityConstraint(objectiveFunction, -rhs, true)
    } else {
      this.maximizeFunction = new PBLConstraint(objectiveFunction, -rhs, true)
    }

    //start to optimize
    while(this.solver.solve(List[Constraint](this.maximizeFunction))){
      //compute the max and min optimum
      this.maxOptimum = this.maximizeFunction.terms.filter(_.l.evaluates2True).map(_.a).sum
      this.minOptimum = this.minimizeFunction.filter(_.l.evaluates2True).map(_.a).sum
      //update the objective function
      this.maximizeFunction.degree = this.maxOptimum + 1
      //reset the solver
      this.solver.reset()
    }

    //return the optimum
    if(this.minOptimum == null){
      None
    } else {
      Some(this.minOptimum)
    }
  }

}
