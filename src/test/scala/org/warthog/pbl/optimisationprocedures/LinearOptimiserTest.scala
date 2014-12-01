package org.warthog.pbl.optimisationprocedures

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.pbl.datastructures.Constraint
import org.warthog.pbl.decisionprocedures.CDCLLike
import org.warthog.pbl.parsers.PBCompetitionReader

/**
 * Tests for the linear search optimiser
 */
class LinearOptimiserTest extends Specification {

  args(sequential = true)

  private def getPBFileString(file: String) =
    List("src", "test", "resources", "pbcompetition", file).mkString(File.separator)

  "Optimisation by linear search  of pseudo-boolean formulas" should {
    "be 3 for formula f10.cnf" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("f10.cnf.opb"))
      val linearOptimiser = new LinearOptimiser()
      linearOptimiser.add(instance._1)
      linearOptimiser.solve(instance._2.get) == Some(3)
    }
    "be 34 for formula mps-v2" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2.opb"))
      val linearOptimiser = new LinearOptimiser()
      linearOptimiser.add(instance._1)
      linearOptimiser.solve(instance._2.get) == Some(34)
    }
    "be 5 for formula v2-stein9" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2-stein9.opb"))
      val linearOptimiser = new LinearOptimiser()
      linearOptimiser.add(instance._1)
      linearOptimiser.solve(instance._2.get) == Some(5)
    }
    "be 9 for formula v2-stein15" in {
      val instance = PBCompetitionReader.getInstance(getPBFileString("normalized-mps-v2-stein15.opb"))
      val linearOptimiser = new LinearOptimiser()
      linearOptimiser.add(instance._1)
      linearOptimiser.solve(instance._2.get) == Some(9)
    }
  }

}
