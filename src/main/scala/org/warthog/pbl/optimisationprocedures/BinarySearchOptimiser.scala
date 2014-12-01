package org.warthog.pbl.optimisationprocedures

import org.warthog.pbl.datastructures.{PBLConstraint, PBLCardinalityConstraint, Constraint, PBLTerm}
import org.warthog.pbl.decisionprocedures.{CDCLLike, Decisionprocedure}

/**
 * Implementation of an optimiser, which uses binary search to compute the optimum
 */
class BinarySearchOptimiser extends Optimisationprocedure{
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


  def solve(objectiveFunction: List[PBLTerm]): Option[BigInt] = {
    //compute the minimize and maximize functions
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

    //determine upper and lower bound for binary search
    var upper = this.maximizeFunction.terms.map(_.a).sum
    var lower = this.maximizeFunction.degree
    var middle = (lower + upper) / 2
    //enforce  objectiveFunction >= middle
    var lowerBoundConstraint = this.maximizeFunction.copy
    lowerBoundConstraint.degree = middle

    //enforce objectiveFunction <= upper
    var upperBoundConstraint = this.computeUpperBoundConstraint(upper)

    while(lower <= upper){
      if(this.solver.solve(List[Constraint](lowerBoundConstraint, upperBoundConstraint))){
        //update max and minOptimum
        this.maxOptimum = this.maximizeFunction.terms.filter(_.l.evaluates2True).map(_.a).sum
        this.minOptimum = this.minimizeFunction.filter(_.l.evaluates2True).map(_.a).sum
        //update the new lower bound
        lower = this.maxOptimum + 1
      } else {
        //update the new upper bound
        upper = middle-1
      }
      middle = (lower + upper) / 2
      //update the constraints
      lowerBoundConstraint = this.maximizeFunction.copy
      lowerBoundConstraint.degree = middle
      upperBoundConstraint = this.computeUpperBoundConstraint(upper)
      solver.reset()
    }

    //return the optimum
    if(this.minOptimum == null){
      None
    } else {
      Some(this.minOptimum)
    }
  }


  /**
   * Method computes the constraint to enforce objective function <= upper bound
   * @param degree the upper bound
   * @return the constraint
   */
  private def computeUpperBoundConstraint(degree: BigInt): Constraint = {
    val terms = this.maximizeFunction.terms.foldLeft(List[PBLTerm]())(_ :+ _.copy)
    terms.map(_.a *= -1)
    //check if constraint is cardinality or not
    if(maximizeFunction.terms.forall(_.a.abs == maximizeFunction.terms(0).a.abs)){
      new PBLCardinalityConstraint(terms, -degree)
    } else {
      new PBLConstraint(terms, -degree)
    }
  }

}
