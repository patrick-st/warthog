package org.warthog.pbl.parsers

import java.io.File

import org.specs2.mutable.Specification
import org.warthog.pbl.datastructures._

/**
 * Created by Patrick on 25.09.2014.
 */
class PBCompetitionReaderTest extends Specification{
val hello = "hello"
  private def getFileString(file: String) =
    List("src", "test", "resources", "pbcompetition", file).mkString(File.separator)

  val correctInstance = PBCompetitionReader.readCompetitionFormat(getFileString("correctFormat.opb"))
  val incorrectInstance = PBCompetitionReader.readCompetitionFormat(getFileString("incorrectFormat.opb"))

  val x1 = new PBLVariable("x1")
  val x2 = new PBLVariable("x2")
  val x3 = new PBLVariable("x3")
  val x4 = new PBLVariable("x4")
  val x5 = new PBLVariable("x5")

  val objectiveFunction = List[PBLTerm](new PBLTerm(1,new PBLLiteral(x1)),
                                        new PBLTerm(1,new PBLLiteral(x2)),
                                        new PBLTerm(1,new PBLLiteral(x3)),
                                        new PBLTerm(1,new PBLLiteral(x4)),
                                        new PBLTerm(1,new PBLLiteral(x5)))

  val terms1 = List[PBLTerm](new PBLTerm(4, new PBLLiteral(x1)),
                             new PBLTerm(3, new PBLLiteral(x2)),
                             new PBLTerm(2, new PBLLiteral(x3)),
                             new PBLTerm(1, new PBLLiteral(x4)))

  val terms2 = List[PBLTerm](new PBLTerm(1, new PBLLiteral(x1)),
                             new PBLTerm(1, new PBLLiteral(x2)),
                             new PBLTerm(1, new PBLLiteral(x4)),
                             new PBLTerm(1, new PBLLiteral(x5)))

  val terms3 = List[PBLTerm](new PBLTerm(1, new PBLLiteral(x3)),
                             new PBLTerm(1, new PBLLiteral(x4)),
                             new PBLTerm(1, new PBLLiteral(x5)))

  val terms4 = List[PBLTerm](new PBLTerm(-1, new PBLLiteral(x1)),
                             new PBLTerm(-2, new PBLLiteral(x2)),
                             new PBLTerm(-3, new PBLLiteral(x3)),
                             new PBLTerm(-4, new PBLLiteral(x4)),
                             new PBLTerm(-5, new PBLLiteral(x5)))

  val terms5_1 = List[PBLTerm](new PBLTerm(-1, new PBLLiteral(x1)),
                             new PBLTerm(1, new PBLLiteral(x2)),
                             new PBLTerm(1, new PBLLiteral(x3)))

  val terms5_2 = List[PBLTerm](new PBLTerm(1, new PBLLiteral(x1)),
                               new PBLTerm(-1, new PBLLiteral(x2)),
                               new PBLTerm(-1, new PBLLiteral(x3)))
  val c1 = new PBLConstraint(terms1, 5)
  val c2 = new PBLCardinalityConstraint(terms2, 2)
  val c3 = new PBLCardinalityConstraint(terms3, 1)
  val c4 = new PBLConstraint(terms4, -2)
  val c5_1 = new PBLCardinalityConstraint(terms5_1, 2)
  val c5_2 = new PBLCardinalityConstraint(terms5_2, -2)


  "correctFormat.opb" should {
    "contain objectiveFunction" in {
      correctInstance._2.get must be equalTo objectiveFunction
    }
    "have 6 constraints" in {
      correctInstance._1.size == 6 must be equalTo true
    }
    "contain c1" in {
      correctInstance._1 must contain(c1)
    }
    "contain c2" in {
      correctInstance._1 must contain(c2)
    }
    "contain c3" in {
      correctInstance._1 must contain(c3)
    }
    "contain c4" in {
      correctInstance._1 must contain(c4)
    }
    "contain c5_1" in {
      correctInstance._1 must contain(c5_1)
    }
    "contain c5_2" in {
      correctInstance._1 must contain(c5_2)
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
      incorrectInstance._1 must contain(c1)
    }
    "contain c2" in {
      incorrectInstance._1 must contain(c2)
    }
    "contain not c3" in {
      incorrectInstance._1.contains(c3) must be equalTo false
    }
    "contain c4" in {
      incorrectInstance._1.contains(c4) must be equalTo false
    }
    "contain c5_1" in {
      incorrectInstance._1 must contain(c5_1)
    }
    "contain c5_2" in {
      incorrectInstance._1 must contain(c5_2)
    }
  }


}
