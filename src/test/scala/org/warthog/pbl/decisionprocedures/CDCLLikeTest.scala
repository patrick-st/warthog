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

  private def getDIMACSFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  private def getPBFileString(file: String) =
    List("src", "test", "resources", "pbl", "opb", file).mkString(File.separator)


  val solver = new CDCLLike(LearnMethod.ClauseLearning)

  "Satisfiability of simple dimacs formulas" should {
    "be true for formula oneClauseFormula" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("oneClauseFormula.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
    "be true for formula oneVariableFormula" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("oneVariableFormula.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
    "be true for formula f01" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f01.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
    "be true for formula f02" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f02.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
    "be false for formula f03" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f03.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f04" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f04.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f05" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f05.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f06" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f06.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f07" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f07.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f08" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f08.cnf"))
      val solver = new CDCLLike(LearnMethod.ClauseLearning)
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f09" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f09.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f10" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f10.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula f11" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f11.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
  }

  "Satisfiability of harder dimacs formulas" should {
    "be true for formula uf150-010" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uf150-010.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
    "be true for formula uf150-027" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uf150-027.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
    "be false for formula uuf150-011" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uuf150-011.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula uuf150-024" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uuf150-024.cnf"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
  }

  "Satisfiability of pseudo-boolean formulas" should {
    "be false for formula normalized-elf.rf6.ucl" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-elf.rf6.ucl.opb"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula normalized-elf.rf7.ucl" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-elf.rf7.ucl.opb"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be false for formula normalized-elf.rf8.ucl" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-elf.rf8.ucl.opb"))
      solver.reset()
      solver.solve(instance._1) == Solver.UNSAT
    }
    "be true for formula normalized-mps-v2" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2.opb"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
    "be true for formula normalized-mps-v2-stein27" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2-stein27.opb"))
      solver.reset()
      solver.solve(instance._1) == Solver.SAT
    }
  }
}
