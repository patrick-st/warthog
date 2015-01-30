package org.warthog.pbl.decisionprocedures

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pbl.parsers.PBCompetitionReader
import org.warthog.pl.decisionprocedures.satsolver.Solver


/**
 * Created by Patrick on 02.10.2014.
 */
class CDCLLikeTest extends Specification {

  args(sequential = true)

  val fs = System.getProperty("file.separator")

  private def getDIMACSFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  private def getPBFileString(directory: String, file: String) =
    List("src", "test", "resources", "pbl", directory, file).mkString(File.separator)


  val CDCLLike_clauseLearning = new CDCLLike(LearnMethod.ClauseLearning)
  val CDCLLike_cardinalityLearning = new CDCLLike(LearnMethod.CardinalityLearning)
  val CDCLLike_pbcLearning = new CDCLLike(LearnMethod.PBConstraintLearning)

  private def testFile(directory: String, file: String, solver: DecisionProcedure, expResult: Int) = {
    val expText = if (expResult == Solver.SAT) "SAT" else "UNSAT"
    "File " + file should {
      "be " + expText in {
        solver.reset()
        val computedResult = if(file.endsWith(".cnf"))
          solver.solve(DIMACSReader.dimacs2PBConstraints(getDIMACSFileString(file))._1)
        else
          solver.solve(PBCompetitionReader.getInstance(getPBFileString(directory, file))._1)
        computedResult must be equalTo expResult
      }
    }
  }

  //test clause learning
  //sipmle cnf formulas
  testFile("", "oneClauseFormula.cnf", CDCLLike_clauseLearning, Solver.SAT)
  testFile("", "oneVariableFormula.cnf", CDCLLike_clauseLearning, Solver.SAT)
  testFile("", "f01.cnf", CDCLLike_clauseLearning, Solver.SAT)
  testFile("", "f02.cnf", CDCLLike_clauseLearning, Solver.SAT)
  testFile("", "f03.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f04.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f05.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f06.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f07.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f08.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f09.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f10.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "f11.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  //harder cnf formulas
  testFile("", "uf150-010.cnf", CDCLLike_clauseLearning, Solver.SAT)
  testFile("", "uf150-027.cnf", CDCLLike_clauseLearning, Solver.SAT)
  testFile("", "uuf150-011.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("", "uuf150-024.cnf", CDCLLike_clauseLearning, Solver.UNSAT)
  //opb files
  testFile("opb", "normalized-elf.rf6.ucl.opb", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("opb", "normalized-elf.rf7.ucl.opb", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("opb", "normalized-elf.rf8.ucl.opb", CDCLLike_clauseLearning, Solver.UNSAT)
  testFile("opb", "normalized-mps-v2.opb", CDCLLike_clauseLearning, Solver.SAT)
  testFile("opb", "normalized-mps-v2-stein27.opb", CDCLLike_clauseLearning, Solver.SAT)

  //test cardinality learning
  testFile("", "oneClauseFormula.cnf", CDCLLike_cardinalityLearning, Solver.SAT)
  testFile("", "oneVariableFormula.cnf", CDCLLike_cardinalityLearning, Solver.SAT)
  testFile("", "f01.cnf", CDCLLike_cardinalityLearning, Solver.SAT)
  testFile("", "f02.cnf", CDCLLike_cardinalityLearning, Solver.SAT)
  testFile("", "f03.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f04.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f05.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f06.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f07.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f08.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f09.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f10.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "f11.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  //harder cnf formulas
  testFile("", "uf150-010.cnf", CDCLLike_cardinalityLearning, Solver.SAT)
  testFile("", "uf150-027.cnf", CDCLLike_cardinalityLearning, Solver.SAT)
  testFile("", "uuf150-011.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("", "uuf150-024.cnf", CDCLLike_cardinalityLearning, Solver.UNSAT)
  //opb files
  testFile("opb", "normalized-elf.rf6.ucl.opb", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("opb", "normalized-elf.rf7.ucl.opb", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("opb", "normalized-elf.rf8.ucl.opb", CDCLLike_cardinalityLearning, Solver.UNSAT)
  testFile("opb", "normalized-mps-v2.opb", CDCLLike_cardinalityLearning, Solver.SAT)
  testFile("opb", "normalized-mps-v2-stein27.opb", CDCLLike_cardinalityLearning, Solver.SAT)

  //test pbc learning
  testFile("", "oneClauseFormula.cnf", CDCLLike_pbcLearning, Solver.SAT)
  testFile("", "oneVariableFormula.cnf", CDCLLike_pbcLearning, Solver.SAT)
  testFile("", "f01.cnf", CDCLLike_pbcLearning, Solver.SAT)
  testFile("", "f02.cnf", CDCLLike_pbcLearning, Solver.SAT)
  testFile("", "f03.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f04.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f05.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f06.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f07.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f08.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f09.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f10.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "f11.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  //harder cnf formulas
  testFile("", "uf150-010.cnf", CDCLLike_pbcLearning, Solver.SAT)
  testFile("", "uf150-027.cnf", CDCLLike_pbcLearning, Solver.SAT)
  testFile("", "uuf150-011.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("", "uuf150-024.cnf", CDCLLike_pbcLearning, Solver.UNSAT)
  //opb files
  testFile("opb", "normalized-elf.rf6.ucl.opb", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("opb", "normalized-elf.rf7.ucl.opb", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("opb", "normalized-elf.rf8.ucl.opb", CDCLLike_pbcLearning, Solver.UNSAT)
  testFile("opb", "normalized-mps-v2.opb", CDCLLike_pbcLearning, Solver.SAT)
  testFile("opb", "normalized-mps-v2-stein27.opb", CDCLLike_pbcLearning, Solver.SAT)
}
