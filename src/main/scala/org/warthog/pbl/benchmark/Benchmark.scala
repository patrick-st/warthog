package org.warthog.pbl.benchmark

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.pbl.datastructures._
import org.warthog.pbl.decisionprocedures.{LearnMethod, CDCLLike}
import org.warthog.pbl.decisionprocedures.LearnMethod.LearnMethod
import org.warthog.pbl.optimisationprocedures.{BranchAndBoundOptimiser, OptimisationProcedure, BinarySearchOptimiser, LinearOptimiser}
import org.warthog.pbl.parsers.PBCompetitionReader
import org.warthog.pl.datastructures.cnf.ImmutablePLClause
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import org.warthog.pl.generators.pbc.BailleuxBoufkhadRoussel
import org.warthog.pl.optimization.maxsat.partialWeighted.{WPM1, BinarySearch, PartialWeightedMaxSATSolver, LinearSearch}
import org.warthog.pl.parsers.maxsat.PartialWeightedMaxSATReader

import scala.collection.mutable

/**
 * Representation of one specific benchmark file
 * @param path of the file
 * @param opt optimiser to test
 * @param learnMethod of the CDCLLike solver
 */
class Benchmark(val path: String, opt: Optimiser.Value, learnMethod: LearnMethod.Value) extends Runnable {

  //the computed optimum
  var minUnsatOptimum: Option[BigInt] = None
  //number of hard and soft clauses
  var hardClauses = -1
  var softClauses = -1

  /**
   * Method runs the chosen optimiser
   */
  def run() = {
    opt match {
      case Optimiser.LinearPBO => executePBLOptimiser(new LinearOptimiser(new CDCLLike(learnMethod)))
      case Optimiser.BinaryPBO => executePBLOptimiser(new BinarySearchOptimiser(new CDCLLike(learnMethod)))
      case Optimiser.BranchAndBoundPBO => executePBLOptimiser(new BranchAndBoundOptimiser)
      case Optimiser.LinearMaxSAT => executeMaxSATOptimiser(new LinearSearch(new Picosat(), BailleuxBoufkhadRoussel))
      case Optimiser.BinaryMaxSAT => executeMaxSATOptimiser(new BinarySearch(new Picosat(), BailleuxBoufkhadRoussel))
      case Optimiser.WPM1 => executeMaxSATOptimiser(new WPM1(new Picosat()))
    }
  }

  /**
   * Method to execute the chosen pbl-optimiser
   * @param optimiser LinearOptimiser, BinarySearchOptimiser or BranchAndBoundOptimiser
   */
  private def executePBLOptimiser(optimiser: OptimisationProcedure) = {
    optimiser.reset()
    val instance = readMaxSATFormatGetPBCFormat(path)
    hardClauses = instance._1.size
    softClauses = instance._2.size
    optimiser.add(instance._1)
    val minUnsat = optimiser.min(instance._2)
    minUnsatOptimum = minUnsat
  }

  /**
   * Method to execute the chosen MaxSAT-Optimiser
   * @param optimiser LinearSearch or BinarySearch
   */
  private def executeMaxSATOptimiser(optimiser: PartialWeightedMaxSATSolver) = {
    optimiser.reset()
    val reader = new PartialWeightedMaxSATReader()
    reader.read(path)
    hardClauses = reader.hardClauses.size
    softClauses = reader.softClauses.size
    optimiser.addHardConstraint(reader.hardClauses)
    val minUnsat = optimiser.solveMinUNSAT(reader.softClauses, reader.weights.toList)
    minUnsat match {
      case Some(opt) => minUnsatOptimum = Some(BigInt(opt))
      case None =>
    }
  }

  /**
   * Method reads a wcnf-file and converts the computed Clauses to PBLConstraints
   * @param path of the wcnf-file
   * @return the hard clauses converted to constraints and the soft clauses (only unit clauses) converted to PBLTerms
   */
  def readMaxSATFormatGetPBCFormat(path: String): (List[Constraint], List[PBLTerm]) = {
    val variables = mutable.HashMap[Int, PBLVariable]()
    val maxSatReader = new PartialWeightedMaxSATReader
    maxSatReader.read(path)
    //convert the hard clauses to PBLConstraints
    val constraints = maxSatReader.hardClauses.toList.foldLeft(List[Constraint]())(_ :+ hardClauseToPBLConstraint(_, variables))
   //convert the soft clauses to PBLTerms
    val zipped = maxSatReader.softClauses zip maxSatReader.weights
    val minFunction = zipped.foldLeft(List[PBLTerm]())(_ :+ unitClauseToPBLTerm(_, variables))
    (constraints, minFunction)
  }

  /**
   * Method converts the ImmutablePLClause to a PBLConstraint
   * @param clause to convert
   * @param variables to enforce that every variable is computed once
   * @return the converted PBLConstraint
   */
  private def hardClauseToPBLConstraint(clause: ImmutablePLClause, variables: mutable.HashMap[Int, PBLVariable]) = {
    val terms = clause.literals.foldLeft(List[PBLTerm]()) { (terms, literal) =>
      val term = if (literal.phase)
        new PBLTerm(1, new PBLLiteral(variables.getOrElseUpdate(literal.variable.name.toInt.abs, new PBLVariable("x" + literal.variable.name))))
      else
        new PBLTerm(1, new PBLLiteral(variables.getOrElseUpdate(literal.variable.name.toInt.abs, new PBLVariable("x" + literal.variable.name)), false))
      terms :+ term
    }
    new PBLCardinalityConstraint(terms, 1)
  }


  /**
   * Method converts an ImmutablePLClause x (has to be unit) and it's weight a to the PBLTerm a* ~x
   * Note: the literal will be negated to avoid an additional computation step
   * If the literal is not negated, the term would represent the maximization function
   * but the minimization function is needed
   * @param clauseWeightPair (clause, weight)
   * @param variables to enforce that every variable is computed once
   * @return a PBLTerm
   */
  private def unitClauseToPBLTerm(clauseWeightPair: (ImmutablePLClause, Long), variables: mutable.HashMap[Int, PBLVariable]) = {
    val clause = clauseWeightPair._1
    val weight = clauseWeightPair._2
    var term: PBLTerm = null
    if (clause.literals.size == 1) {
      //Note the negated literal will be computed to obtain a minimize function
      if (!clause.literals(0).phase)
        term = new PBLTerm(BigInt(weight), new PBLLiteral(variables.getOrElseUpdate(clause.literals(0).variable.name.toInt.abs, new PBLVariable("x" + clause.literals(0).variable.name))))
      else
        term = new PBLTerm(BigInt(weight), new PBLLiteral(variables.getOrElseUpdate(clause.literals(0).variable.name.toInt.abs, new PBLVariable("x" + clause.literals(0).variable.name)), false))
    } else
      System.err.println("Can't convert clause to PBLTerm because clause is not unit clause")
    term
  }
}

object Optimiser extends Enumeration {
  type Optimiser = Value
  val LinearPBO, BinaryPBO, BranchAndBoundPBO, LinearMaxSAT, BinaryMaxSAT, WPM1 = Value
}

