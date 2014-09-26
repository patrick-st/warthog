package org.warthog.pbl

import org.warthog.pbl.datastructures._

/**
 * Collection of pseudo-boolean constraint, terms, literals and variables
 */
object C {

  //variables
  val x1 = new PBLVariable("x1")
  val x1_1 = new PBLVariable("x1")
  val x2 = new PBLVariable("x2")
  val x3 = new PBLVariable("x3")
  val x4 = new PBLVariable("x4")
  val x5 = new PBLVariable("x5")

  //literals
  val l1 = new PBLLiteral(x1)
  val l1_1 = new PBLLiteral(x1)
  val l2 = new PBLLiteral(x2)
  val l3 = new PBLLiteral(x3)
  val l4 = new PBLLiteral(x4)
  val l5 = new PBLLiteral(x5)

  val l3_ = new PBLLiteral(x3, false)
  val l4_ = new PBLLiteral(x4, false)


  //terms




  //constraints
  val terms1 = List[PBLTerm](
    new PBLTerm(4, l1.copy),
    new PBLTerm(3, l2.copy),
    new PBLTerm(2, l3.copy),
    new PBLTerm(1, l4.copy))

  val terms2 = List[PBLTerm](
    new PBLTerm(1, l1.copy),
    new PBLTerm(1, l2.copy),
    new PBLTerm(1, l4.copy),
    new PBLTerm(1, l5.copy))

  val terms3 = List[PBLTerm](
    new PBLTerm(1, l3.copy),
    new PBLTerm(1, l4.copy),
    new PBLTerm(1, l5.copy))

  val terms4 = List[PBLTerm](
    new PBLTerm(-1, l1.copy),
    new PBLTerm(-2, l2.copy),
    new PBLTerm(-3, l3.copy),
    new PBLTerm(-4, l4.copy),
    new PBLTerm(-5, l5.copy))

  val terms5_1 = List[PBLTerm](
    new PBLTerm(-1, l1.copy),
    new PBLTerm(1, l2.copy),
    new PBLTerm(1, l3.copy))

  val terms5_2 = List[PBLTerm](
    new PBLTerm(1, l1.copy),
    new PBLTerm(-1, l2.copy),
    new PBLTerm(-1, l3.copy))

  val c1 = new PBLConstraint(terms1, 5)
  val c2 = new PBLCardinalityConstraint(terms2, 2)
  val c3 = new PBLCardinalityConstraint(terms3, 1)
  val c4 = new PBLConstraint(terms4, -2)
  val c5_1 = new PBLCardinalityConstraint(terms5_1, 2)
  val c5_2 = new PBLCardinalityConstraint(terms5_2, -2)


  //objective function
  val objectiveFunction = List[PBLTerm](
    new PBLTerm(1,new PBLLiteral(x1)),
    new PBLTerm(1,new PBLLiteral(x2)),
    new PBLTerm(1,new PBLLiteral(x3)),
    new PBLTerm(1,new PBLLiteral(x4)),
    new PBLTerm(1,new PBLLiteral(x5)))
}

