package org.warthog.pbl.decisionprocedures

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.MiniSatJava

/**
 * Created by Patrick on 02.10.2014.
 */
class CDCLLikeTest extends Specification {

  args(sequential = true)

  private def getFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  "Satisfiability of simple dimacs formulas" should {
    "be true for formula oneClauseFormula" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("oneClauseFormula.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beTrue
    }
    "be true for formula oneVariableFormula" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("oneVariableFormula.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beTrue
    }
    "be true for formula f01" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f01.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beTrue
    }
    "be true for formula f02" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f02.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beTrue
    }
    "be false for formula f03" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f03.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f04" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f04.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f05" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f05.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f06" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f06.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f07" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f07.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f08" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f08.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f09" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f09.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f10" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f10.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula f11" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("f11.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
  }

  "Satisfiability of harder dimacs formulas" should {
    "be true for formula uf150-010" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("uf150-010.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beTrue
    }
    "be true for formula uf150-027" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("uf150-027.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beTrue
    }
    "be false for formula uuf150-011" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("uuf150-011.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
    "be false for formula uuf150-024" in {
      val instance = DIMACSReader.dimacs2PBConstraints(getFileString("uuf150-024.cnf"))
      val solver = new CDCLLike(instance._1, instance._2)
      solver.solve must beFalse
    }
  }
}
