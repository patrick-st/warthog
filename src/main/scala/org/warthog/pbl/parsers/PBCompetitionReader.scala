package org.warthog.pbl.parsers

import org.warthog.pbl.datastructures._

/**
 * Parser to read pseudo-boolean constraint instances
 * in Pseudo-boolean Competition format
 */
object PBCompetitionReader {


  def readCompetitionFormat (path: String): (List[Constraint], List[PBLTerm]) = {
    var preambleRead = false
    var numberOfConstraintsInPreamble = 0
    var numberOfVarsInPreamble = 0
    var numberOfEqualityConstraints = 0

    val lines = io.Source.fromFile(path).getLines()
    var lineNumber = 0

    var constraints: List[Constraint] = List[Constraint]()
    var objectiveFunction = List[PBLTerm]()

    while (lines.hasNext) {
      lineNumber += 1
      val line = lines.next().trim.replaceAll("\\s+", " ")

      if (!line.isEmpty) {
        line(0) match {
          case '*' =>
            if (!preambleRead && line.contains("#variable=")) {
              val tokens = line.drop(1).trim.split("#variable=|#constraint=")
              numberOfVarsInPreamble = tokens(1).trim.toInt
              numberOfConstraintsInPreamble = tokens(2).trim.toInt
              preambleRead = true
            } else if(preambleRead && line.contains("#variable="))
              System.err.println("Line " + lineNumber + ": More than one preamble --> Use the first")
          case 'm' => objectiveFunction = parseObjective(line)
          case _ => parseConstraint(line) match{
            case (c, None) => constraints :+= c
            case (c1, Some(c2)) => constraints :+= c1; constraints :+= c2; numberOfEqualityConstraints +=1
          }
        }
      }
    }

    val vars = constraints.foldLeft(Set[String]())(_ ++ _.terms.foldLeft(Set[String]())(_ + _.l.v.name))
    if (preambleRead) {
      if (numberOfConstraintsInPreamble+numberOfEqualityConstraints != constraints.size)
        System.err.println("Number of Clauses in Preamble: " + numberOfConstraintsInPreamble + ", " + "Number of computed Clauses: " + constraints.size)
      if (numberOfVarsInPreamble != vars.size)
        System.err.println("Number of Vars in Preamble: " + numberOfVarsInPreamble + ", " + "Number of computed Vars: " + vars.size)
    }
    (constraints, objectiveFunction)
  }


  /**
   *  Parses a string that represents a constraint and returns:
   *  - case '>=' constraint: a tuple (the constraint, None)
   *  - case '=' constraint: a tuple ('>=' constraint, Some('<=' constraint))
   * @param line string representation of a constraint
   * @return the generated constraint
   */
  def parseConstraint(line: String) : (Constraint, Option[Constraint]) ={
   //check if the line contains an '>=' or '=' constraint
    val equalOperator = !line.contains(">=")
    //split the line into left-hand and right-hand side
    val splittedLine = line.split(">=|=")
    val lhs = splittedLine(0).trim
    //remove ';' of right-hand side
    val rhs = splittedLine(1).trim.dropRight(1).trim
    val termRegex = "[-|+]?\\s*\\d+\\s*x\\d+".r
    //compute the terms of the '>=' constraint
    val terms = termRegex.findAllIn(lhs)
    val termList = terms.foldLeft(List[PBLTerm]())(_ :+ string2Term(_))

    //compute the '>=' constraint
    var constraint: Constraint = new PBLConstraint(termList,BigInt(rhs))
    if(constraint.asInstanceOf[PBLConstraint].isCardinalityConstraint())
      constraint = new PBLCardinalityConstraint(constraint.terms, constraint.degree)
    //compute the <= constraint if necessary
    val option = if(equalOperator) {
      val lessOrEqualConstraint = constraint.copy
      lessOrEqualConstraint * -1
      lessOrEqualConstraint.normalize()
      Some(lessOrEqualConstraint)
    } else {
      None
    }

    (constraint, option)
  }


  private def parseObjective(line: String) = {
    val termRegex = "[-|+]?\\s*\\d+\\s*x\\d+".r
    val terms = termRegex.findAllIn(line.replace("min:",""))
    terms.foldLeft(List[PBLTerm]())(_ :+ string2Term(_))
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
    val startTime = System.currentTimeMillis()
    //val pair = PBCompetitionReader.readCompetitionFormat("C:\\Users\\Patrick\\Desktop\\Masterarbeit\\PB competition instances\\PB11\\normalized-PB11\\DEC-SMALLINT-LIN\\lopes\\normalized-184.opb")
    val elapsedTime = System.currentTimeMillis()-startTime
    println("Time in milli: " + elapsedTime)
    println("Time in min: " + elapsedTime/60000)
    //println("objective function: " + pair._2)
    println()
    println("Constraints: ")
    //pair._1.foreach(println(_))
    print(PBCompetitionReader.parseConstraint("2 x1 2 x2 -2 x3 = 2 ;"))
  }

}
