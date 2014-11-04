package org.warthog.pbl.decisionprocedures

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pbl.parsers.PBCompetitionReader
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.MiniSatJava

/**
 * Created by Patrick on 02.10.2014.
 */
class CDCLLikeTest extends Specification {

  args(sequential = true)

  private def getDIMACSFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  private def getPBFileString(file: String) =
    List("src", "test", "resources", "pbcompetition", file).mkString(File.separator)

  "Satisfiability of simple dimacs formulas" should {
    "be true for formula oneClauseFormula" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("oneClauseFormula.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beTrue
    }
    "be true for formula oneVariableFormula" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("oneVariableFormula.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beTrue
    }
    "be true for formula f01" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f01.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beTrue
    }
    "be true for formula f02" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f02.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beTrue
    }
    "be false for formula f03" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f03.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f04" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f04.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f05" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f05.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f06" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f06.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f07" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f07.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f08" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f08.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f09" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f09.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f10" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f10.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f11" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("f11.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
  }

  "Satisfiability of harder dimacs formulas" should {
    "be true for formula uf150-010" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uf150-010.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beTrue
    }
    "be true for formula uf150-027" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uf150-027.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beTrue
    }
    "be false for formula uuf150-011" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uuf150-011.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
    "be false for formula uuf150-024" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getDIMACSFileString("uuf150-024.cnf"))
      val solver = new CDCLLike(instance._1, None, instance._2)
      solver.solve must beFalse
    }
  }


  "Satisfiability of pseudo-boolean formulas" should {
    "be false for formula normalized-elf.rf6.ucl" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-elf.rf6.ucl.opb"))
      val solver = new CDCLLike(instance._1,instance._2, instance._3)
      solver.solve must beFalse
    }
    "be false for formula normalized-elf.rf7.ucl" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-elf.rf7.ucl.opb"))
      val solver = new CDCLLike(instance._1,instance._2, instance._3)
      solver.solve must beFalse
    }
    "be false for formula normalized-elf.rf8.ucl" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-elf.rf8.ucl.opb"))
      val solver = new CDCLLike(instance._1,instance._2, instance._3)
      solver.solve must beFalse
    }
  }

  "Optimisation by linear search  of pseudo-boolean formulas" should {
    "be 6 for formula f10.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f10.cnf.opb"))
      val solver = new CDCLLike(instance._1, instance._2, instance._3)
      solver.linearSearchOptimisation()
      solver.optimum == 6
    }
  }


  "Optimisation by binary search  of pseudo-boolean formulas" should {
    "be 6 for formula f10.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f10.cnf.opb"))
      val solver = new CDCLLike(instance._1, instance._2, instance._3)
      solver.binarySearchOptimisation()
      solver.optimum == 6
    }
  }
}
