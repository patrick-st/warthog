package org.warthog.pbl.parsers

import org.warthog.pbl.datastructures._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Parser to read pseudo-boolean constraint instances
 * in Pseudo-boolean Competition format
 */
object PBCompetitionReader {
  var variables = new mutable.HashMap[Int, PBLVariable]()

  /**
   * Read a File in Pseudo-Boolean Competition format
   * @param path path of the source
   * @return a pair containing a list of all constraints an the objective function
   */
  def readCompetitionFormat(path: String): (List[Constraint], Option[List[PBLTerm]]) = {
    var preambleRead = false
    var numberOfConstraintsInPreamble = 0
    var numberOfVarsInPreamble = 0
    var numberOfEqualityConstraints = 0

    val lines = io.Source.fromFile(path).getLines()
    var lineNumber = 0

    var constraints = ListBuffer[Constraint]()
    var objectiveFunction: Option[List[PBLTerm]] = None

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
            } else if (preambleRead && line.contains("#variable="))
              System.err.println("Line " + lineNumber + ": More than one preamble --> Use the first")
          case 'm' =>
            if(line.endsWith(";")) objectiveFunction = Some(parseObjective(line))
            else System.err.println("Line " + lineNumber + ": Line doesn't end with ';' --> Skip line")
          case _ =>
            if(line.endsWith(";")){
              parseConstraint(line) match {
                case (c, None) => constraints += c
                case (c1, Some(c2)) => constraints += c1; constraints += c2; numberOfEqualityConstraints += 1
              }
            } else System.err.println("Line " + lineNumber + ": Line doesn't end with ';' --> Skip line")
        }
      }
    }

    val vars = this.variables.size
    if (preambleRead) {
      if (numberOfConstraintsInPreamble + numberOfEqualityConstraints != constraints.size)
        System.err.println("Number of Clauses in Preamble: " + numberOfConstraintsInPreamble + ", " + "Number of computed Clauses: " + constraints.size)
      if (numberOfVarsInPreamble != vars)
        System.err.println("Number of Vars in Preamble: " + numberOfVarsInPreamble + ", " + "Number of computed Vars: " + vars)
    }
    (constraints.toList, objectiveFunction)
  }


  /**
   * Parses a string that represents a constraint and returns:
   * - case '>=' constraint: a tuple (the constraint, None)
   * - case '=' constraint: a tuple ('>=' constraint, Some('<=' constraint))
   * @param line string representation of a constraint
   * @return the generated constraint
   */
  private def parseConstraint(line: String): (Constraint, Option[Constraint]) = {
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
    val termList = terms.foldLeft(ListBuffer[PBLTerm]())(_ += string2Term(_)).toList

    //compute the '>=' constraint
    var constraint: Constraint = new PBLConstraint(termList, BigInt(rhs))
    if (constraint.asInstanceOf[PBLConstraint].isCardinalityConstraint())
      constraint = new PBLCardinalityConstraint(constraint.terms, constraint.degree)
    //compute the <= constraint if necessary
    val option = if (equalOperator) {
      val lessOrEqualConstraint = constraint.copy
      lessOrEqualConstraint * -1
      lessOrEqualConstraint.normalize()
      Some(lessOrEqualConstraint)
    } else {
      None
    }
    (constraint, option)
  }

  /**
   * Parse the objective function
   * @param line string representation of the objective function
   * @return a list of terms
   */
  private def parseObjective(line: String) = {
    val termRegex = "[-|+]?\\s*\\d+\\s*x\\d+".r
    val terms = termRegex.findAllIn(line.replace("min:", ""))
    terms.foldLeft(ListBuffer[PBLTerm]())(_ += string2Term(_)).toList
  }

  /**
   * Converts a string representation of a term into a PBLTerm
   * @param term the string representation of a term
   * @return
   */
  private def string2Term(term: String): PBLTerm = {
    val splitted = term.trim.split("\\s+")
    var variableName = ""
    var variableID = 0
    var termCoefficient: BigInt = 0
    //case term matches +|-\\s+x\\s+\\d+
    if (splitted.size == 3) {
      variableName = splitted(2).trim
      termCoefficient = BigInt(splitted(0) + splitted(1))
      //case term = +|-x\\s+\\d+
    } else {
      variableName = splitted(1)
      termCoefficient = BigInt(splitted(0))
    }
    variableID = variableName.drop(1).toInt
    variables.get(variableID) match {
      case None =>
        //compute new variable an add the variable to the HashMap
        val vari: PBLVariable = new PBLVariable(variableName)
        variables += variableID -> vari
        new PBLTerm(termCoefficient, new PBLLiteral(vari))
      case Some(v) => new PBLTerm(termCoefficient, new PBLLiteral(v))
    }
  }
}


object Main {
  def main(args: Array[String]) {
    val pair = PBCompetitionReader.readCompetitionFormat("path_of_file")
  }

}
