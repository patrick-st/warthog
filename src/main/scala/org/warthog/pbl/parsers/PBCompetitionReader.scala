package org.warthog.pbl.parsers

import org.warthog.pbl.datastructures._

/**
 * Created by Patrick on 10.09.2014.
 */
object PBCompetitionReader {


  def parseConstraint(line: String) : Constraint ={
    val equalOperator = !line.contains(">=")
    val splittedLine = line.trim.replace("\\s+", " ").split(">=|=")
    val lhs = splittedLine(0).trim
    //remove ; of right-hand side
    val rhs = splittedLine(1).trim.dropRight(1).trim
    val termRegex = "[-|+]?\\s*\\d+\\s*x\\d+".r
    val terms = termRegex.findAllIn(lhs)
    val termList = terms.foldLeft(List[PBLTerm]())(_ :+ string2Term(_))
    if(equalOperator) {

    } else {

    }
    new PBLConstraint(termList,BigInt(rhs))
  }


  private def string2Term(term: String): PBLTerm = {
    val splitted = term.trim.split("\\s+")
    if(splitted.size == 3){
      new PBLTerm(BigInt(splitted(0)+ splitted(1)),new PBLLiteral(new PBLVariable(splitted(2).trim)))
    } else {
      new PBLTerm(BigInt(splitted(0)), new PBLLiteral(new PBLVariable(splitted(1).trim)))
    }
  }





}


object Main {
  def main(args: Array[String]) {

    val constr = PBCompetitionReader.parseConstraint("1         x1           -3      x2 +8 x3 >=  123 ;")
    println(constr)
    println(constr.asInstanceOf[PBLConstraint].isCardinalityConstraint())
    println(constr)
    println()
    println()

    val constr2 = PBCompetitionReader.parseConstraint("2 x1 +2 x2 + 2 x3  >= 123 ;")
    println(constr2)
    println(constr2.asInstanceOf[PBLConstraint].isCardinalityConstraint())
    println(constr2)
    println()
    val termi = constr2.terms.map{x => x}
    println(new PBLCardinalityConstraint(termi, constr2.degree))
    println(constr2)


  }

}
