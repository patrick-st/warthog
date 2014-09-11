package org.warthog.pbl.parsers

import org.warthog.pbl.datastructures._

/**
 * Parser to read pseudo-boolean constraint instances
 * in Pseudo-boolean Competition format
 */
object PBCompetitionReader {

  /**
   *  Parses a string that represents a constraint and returns:
   *  - case 
   * @param line string representation of a constraint
   * @return the generated constraint
   */
  def parseConstraint(line: String) : (Constraint, Option[Constraint]) ={
   //check if the line contains an '>=' or '=' constraint
    val equalOperator = !line.contains(">=")
    //split the line into left-hand and right-hand side
    val splittedLine = line.trim.replace("\\s+", " ").split(">=|=")
    val lhs = splittedLine(0).trim
    //remove ';' of right-hand side
    val rhs = splittedLine(1).trim.dropRight(1).trim
    val termRegex = "[-|+]?\\s*\\d+\\s*x\\d+".r
    //compute the terms of the '>=' constraint
    val terms = termRegex.findAllIn(lhs)
    val termList = terms.foldLeft(List[PBLTerm]())(_ :+ string2Term(_))
    //compute the <= constraint if necessary
    val lessOrEqualConstraint = if(equalOperator) {
      val termList2 = terms.foldLeft(List[PBLTerm]())(_ :+ string2Term(_))
      termList2.map(_.a *= -1)
      Some(new PBLConstraint(termList2, BigInt(rhs)))
    } else {
      None
    }
    (new PBLConstraint(termList,BigInt(rhs)), lessOrEqualConstraint)
  }


  /**
   * Converts a string representation of a term into a PBLTerm
   * @param term the string representation of a term
   * @return
   */
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
