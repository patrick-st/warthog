package org.warthog.pbl.parsers

import org.warthog.pbl.datastructures._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Parser to read pseudo-boolean constraint instances
 * in Pseudo-boolean Competition format
 * Reference: Input/Output Format and Solver Requirements
 * for the Competitions of Pseudo-Boolean Solvers
 * http://www.cril.univ-artois.fr/PB12/format.pdf
 */
object PBCompetitionReader {

  def getInstance(path: String) = {
    val instance = readCompetitionFormat(path)
    (instance._1, instance._2, instance._3)
  }

  /**
   * Read a File in Pseudo-Boolean Competition format
   * @param path path of the source
   * @return a pair containing a list of all constraints an the objective function
   */
  def readCompetitionFormat(path: String): (List[Constraint], Option[List[PBLTerm]], mutable.HashMap[Int, PBLVariable]) = {
    val variables = new mutable.HashMap[Int, PBLVariable]()
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
            if (line.endsWith(";")) objectiveFunction = Some(parseObjective(line, variables))
            else System.err.println("Line " + lineNumber + ": Line doesn't end with ';' --> Skip line")
          case _ =>
            if (line.endsWith(";")) {
              try {
                parseConstraint(line, variables) match {
                  case (c, None) => constraints += c
                  case (c1, Some(c2)) => constraints += c1; constraints += c2; numberOfEqualityConstraints += 1
                }
              } catch {
                case e: NumberFormatException => System.err.println("Line " + lineNumber + ": Number format exception --> Skip line")
              }
            } else System.err.println("Line " + lineNumber + ": Line doesn't end with ';' --> Skip line")
        }
      }
    }

    val vars = variables.size
    if (preambleRead) {
      if (numberOfConstraintsInPreamble + numberOfEqualityConstraints != constraints.size)
        System.err.println("Number of Clauses in Preamble: " + numberOfConstraintsInPreamble + ", " + "Number of computed Clauses: " + (constraints.size - numberOfEqualityConstraints))
      if (numberOfVarsInPreamble != vars)
        System.err.println("Number of Vars in Preamble: " + numberOfVarsInPreamble + ", " + "Number of computed Vars: " + vars)
    }
    (constraints.toList, objectiveFunction, variables)
  }

  /**
   * Parses a string that represents a constraint and returns:
   * - case '>=' constraint: a tuple (the constraint, None)
   * - case '=' constraint: a tuple ('>=' constraint, Some('<=' constraint))
   * @param line string representation of a constraint
   * @return the generated constraint
   */
  private def parseConstraint(line: String, variables: mutable.HashMap[Int, PBLVariable]): (Constraint, Option[Constraint]) = {
    //check if the line contains an '>=' or '=' constraint
    val equalOperator = !line.contains(">=")
    //split the line into left-hand and right-hand side
    val splittedLine = line.split(">=|=")
    val lhs = splittedLine(0).trim
    //remove ';' of right-hand side
    val rhs = splittedLine(1).trim.dropRight(1).trim
    val termRegex = "[-|+]?\\s*\\d+\\s*x\\s*\\d+".r
    //compute the terms of the '>=' constraint
    val terms = termRegex.findAllIn(lhs).duplicate
    val termList = terms._1.foldLeft(ListBuffer[PBLTerm]()) {
      try {
        _ += string2Term(_, variables)
      } catch {
        case e: NumberFormatException => throw e
      }
    }.toList

    //compute the '>=' constraint
    val isCardinality = isCardinalityConstraint(termList)
    val constraint = if (isCardinality) {
      new PBLCardinalityConstraint(termList, BigInt(rhs))
    } else {
      var c: Constraint = new PBLConstraint(termList, BigInt(rhs))
      c.saturation()
      //check if the constraint is cardinality after saturation
      if (isCardinalityConstraint(c.terms))
        c = new PBLCardinalityConstraint(c.terms, c.degree)
      c
    }
    //compute the <= constraint if necessary
    val option = if (equalOperator) {
      val termList2 = terms._2.foldLeft(ListBuffer[PBLTerm]()) {
        try {
          _ += string2Term(_, variables)
        } catch {
          case e: NumberFormatException => throw e
        }
      }.toList
      //multiply the constraint with -1
      termList2.map(_.a *= -1)
      val lessOrEqualConstraint = if (isCardinality) {
        new PBLCardinalityConstraint(termList2, -BigInt(rhs))
      } else {
        var c: Constraint = new PBLConstraint(termList2, -BigInt(rhs))
        c.saturation()
        //check if the constraint is cardinality after saturation
        if (isCardinalityConstraint(c.terms))
          c = new PBLCardinalityConstraint(c.terms, c.degree)
        c
      }
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
  private def parseObjective(line: String, variables: mutable.HashMap[Int, PBLVariable]) = {
    val termRegex = "[-|+]?\\s*\\d+\\s*x\\d+".r
    val terms = termRegex.findAllIn(line.replace("min:", ""))
    terms.foldLeft(ListBuffer[PBLTerm]())(_ += string2Term(_, variables)).toList
  }

  /**
   * Converts a string representation of a term into a PBLTerm
   * @param term the string representation of a term
   * @return
   */
  private def string2Term(term: String, variables: mutable.HashMap[Int, PBLVariable]): PBLTerm = {
    val splitted = term.trim.split("\\s+")
    var variableName = ""
    var variableID = 0
    var termCoefficient: BigInt = 0
    //case term matches +|-\\s+\\d+\\s+x\\d+
    if (splitted.size == 3) {
      variableName = splitted(2).trim
      try {
        termCoefficient = BigInt(splitted(0) + splitted(1))
      } catch {
        case e: NumberFormatException => throw e
      }
      //case term = +|-\\d+\\s+x\\d+
    } else {
      variableName = splitted(1)
      try {
        termCoefficient = BigInt(splitted(0))
      } catch {
        case e: NumberFormatException => throw e
      }
    }
    variableID = variableName.drop(1).toInt
    new PBLTerm(termCoefficient, new PBLLiteral(variables.getOrElseUpdate(variableID, new PBLVariable(variableName))))
  }

  /**
   * Checks if the constraint is a cardinality constraint
   * @param terms the terms of the constraint
   * @return true if the constraint is a cardinality constraint else false
   */
  private def isCardinalityConstraint(terms: List[PBLTerm]): Boolean = {
    val coefficient = terms(0).a.abs
    terms.forall(_.a.abs == coefficient)
  }
}



