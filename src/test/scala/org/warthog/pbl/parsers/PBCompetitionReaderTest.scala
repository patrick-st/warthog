package org.warthog.pbl.parsers

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.pbl.C
import org.warthog.pbl.datastructures._

/**
 * Tests for the PBCompetitionReader
 */
class PBCompetitionReaderTest extends Specification{
val hello = "hello"
  private def getFileString(file: String) =
    List("src", "test", "resources", "pbcompetition", file).mkString(File.separator)

  val correctInstance = PBCompetitionReader.readCompetitionFormat(getFileString("correctFormat.opb"))
  val incorrectInstance = PBCompetitionReader.readCompetitionFormat(getFileString("incorrectFormat.opb"))








  "correctFormat.opb" should {
    "contain objectiveFunction" in {
      correctInstance._2.get must be equalTo C.objectiveFunction
    }
    "have 6 constraints" in {
      correctInstance._1.size == 6 must be equalTo true
    }
    "contain c1" in {
      correctInstance._1 must contain(C.c1)
    }
    "contain c2" in {
      correctInstance._1 must contain(C.c2)
    }
    "contain c3" in {
      correctInstance._1 must contain(C.c3)
    }
    "contain c4" in {
      correctInstance._1 must contain(C.c4)
    }
    "contain c5_1" in {
      correctInstance._1 must contain(C.c5_1)
    }
    "contain c5_2" in {
      correctInstance._1 must contain(C.c5_2)
    }
  }

  "incorrectFormat.opb" should {
    "contain an empty objective function" in {
      incorrectInstance._2.get must be equalTo List[PBLTerm]()
    }
    "have 4 constraints" in {
      incorrectInstance._1.size == 4 must be equalTo true
    }
    "contain c1" in {
      incorrectInstance._1 must contain(C.c1)
    }
    "contain c2" in {
      incorrectInstance._1 must contain(C.c2)
    }
    "contain not c3" in {
      incorrectInstance._1.contains(C.c3) must be equalTo false
    }
    "contain c4" in {
      incorrectInstance._1.contains(C.c4) must be equalTo false
    }
    "contain c5_1" in {
      incorrectInstance._1 must contain(C.c5_1)
    }
    "contain c5_2" in {
      incorrectInstance._1 must contain(C.c5_2)
    }
  }


}
