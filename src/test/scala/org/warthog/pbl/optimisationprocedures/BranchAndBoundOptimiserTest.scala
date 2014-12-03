package org.warthog.pbl.optimisationprocedures

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.pbl.parsers.PBCompetitionReader

/**
 * Tests for the branch and bound optimisation method
 */
class BranchAndBoundOptimiserTest extends Specification{

  args(sequential = true)

  private def getPBFileString(file: String) =
    List("src", "test", "resources", "pbcompetition", file).mkString(File.separator)

  "Optimisation by binary search  of simple cnf formulas" should {
    "be 0 for formula f01.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f01.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(0)
    }
    "be 0 for formula f02.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f02.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(0)
    }
    "be 1 for formula f03.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f03.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(1)
    }
    "be 1 for formula f04.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f04.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(1)
    }
    "be 1 for formula f05.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f05.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(1)
    }
    "be 1 for formula f06.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f06.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(1)
    }
    "be 2 for formula f07.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f07.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(2)
    }
    "be 5 for formula f08.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f08.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(5)
    }
    "be 3 for formula f09.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f09.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(3)
    }
    "be 3 for formula f10.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f10.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(3)
    }
    "be 2 for formula f11.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f11.cnf.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(2)
    }
  }

  "Optimisation by binary search of pseudo-boolean formulas" should {
    "be 34 for formula mps-v2" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(34)
    }
    "be 5 for formula v2-stein9" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2-stein9.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(5)
    }
    "be 9 for formula v2-stein15" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2-stein15.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(9)
    }
    "be 3089 for formula v2-20-10" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2-20-10.opb"))
      val optimiser = new BranchAndBoundOptimiser()
      optimiser.add(instance._1)
      optimiser.solve(instance._2.get) == Some(3089)
    }
  }

}
