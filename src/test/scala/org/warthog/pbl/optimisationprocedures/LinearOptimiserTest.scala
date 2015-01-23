package org.warthog.pbl.optimisationprocedures

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.pbl.C
import org.warthog.pbl.datastructures.Constraint
import org.warthog.pbl.decisionprocedures.{LearnMethod, CDCLLike}
import org.warthog.pbl.parsers.PBCompetitionReader

/**
 * Tests for the linear search optimiser
 */
class LinearOptimiserTest extends Specification {

  args(sequential = true)

  val fs = System.getProperty("file.separator")

  private def getPBFileString(file: String) =
    List("src", "test", "resources", "pbl", file).mkString(File.separator)

  "Optimisation by linear search  of simple cnf formulas" should {
    "be 0 for formula f01.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f01.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(0)
    }
    "be 0 for formula f02.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f02.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(0)
    }
    "be 1 for formula f03.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f03.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(1)
    }
    "be 1 for formula f04.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f04.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(1)
    }
    "be 1 for formula f05.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f05.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(1)
    }
    "be 1 for formula f06.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f06.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(1)
    }
    "be 2 for formula f07.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f07.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(2)
    }
    "be 5 for formula f08.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f08.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(5)
    }
    "be 2 for formula f08.cnf with new objective function" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f08.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get)
      linearOptimiser.min(C.f08_objectiveFunction.map(_.copy)) == Some(2)
    }
    "be 3 for formula f09.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f09.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(3)
    }
    "be 3 for formula f10.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f10.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(3)
    }
    "be 2 for formula f11.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("cnf" + fs + "f11.cnf.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(2)
    }
  }

  "Optimisation by linear search  of simple pseudo-boolean formulas" should {
    "be 34 for formula mps-v2" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("opb" + fs + "normalized-mps-v2.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(34)
    }
    "be 5 for formula v2-stein9" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("opb" + fs + "normalized-mps-v2-stein9.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(5)
    }
    "be 9 for formula v2-stein15" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("opb" + fs + "normalized-mps-v2-stein15.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(9)
    }
    "be 3089 for formula v2-20-10" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("opb" + fs + "normalized-mps-v2-20-10.opb"))
      val linearOptimiser = new LinearOptimiser(new CDCLLike(LearnMethod.ClauseLearning))
      linearOptimiser.add(instance._1)
      linearOptimiser.min(instance._2.get) == Some(3089)
    }
  }
}
