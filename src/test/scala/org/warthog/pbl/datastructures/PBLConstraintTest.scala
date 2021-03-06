package org.warthog.pbl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pbl.C

import scala.collection.mutable

/**
 * Test for PBLConstraint
 */
class PBLConstraintTest extends Specification {
  sequential
  C.x1.state = State.OPEN
  C.x1_1.state = State.OPEN
  C.x2.state = State.OPEN
  C.x3.state = State.OPEN
  C.x4.state = State.OPEN
  C.x5.state = State.OPEN

  "c1" should {
    "be equal to c1" in {
      C.c1 must be equalTo C.c1
    }
    "be equal to c1_1" in {
      C.c1 must be equalTo C.c1_1
    }
    "not be equal to c1_2" in {
      C.c1 == C.c1_2 must be equalTo false
    }
    "not be equal to c1_3" in {
      C.c1 == C.c1_3 must be equalTo false
    }
    "not be equal to c2" in {
      C.c1 == C.c2 must be equalTo false
    }
    "be equal to c2Cardinality after reducing to cardinality constraint" in {
      C.c1.toCardinalityConstraint == C.c1Cardinality must be equalTo true
    }
  }


  "After initializing the watched literals, c1" should {
    "have the ConstraintState SUCCESS" in {
      val state = C.c1.initWatchedLiterals
      state must be equalTo ConstraintState.UNRESOLVED
    }
    "have an initial slack of 5" in {
      C.c1.slack must be equalTo 5
    }
    "have an initial currentSum of 0" in {
      C.c1.currentSum must be equalTo 0
    }

  }

  val units = mutable.HashSet[Constraint]()
  "After assigning x1 to false, c1" should {
    "be unit" in {
      C.x1.assign(false, units, 1, null)
      units.contains(C.c1) must be equalTo true
    }
    "have a slack of 1" in {
      C.c1.slack must be equalTo 1
    }
    "have a currentSum of 0" in {
      C.c1.currentSum must be equalTo 0
    }
  }

  "After assigning x2 to false, c1" should {
    "be empty" in {
      C.x2.assign(false, units, 1, null) == Some(C.c1)
    }
    "have a slack of -2" in {
      C.c1.slack must be equalTo -2
    }
    "have a currentSum of 0" in {
      C.c1.currentSum must be equalTo 0
    }
  }

  "After initializing the watched literals, emptyConstraint" should {
    "be empty" in {
      C.emptyConstraint.initWatchedLiterals must be equalTo ConstraintState.EMPTY
    }
  }

  "After initializing the watched literals, unitConstraint" should {
    "be unit" in {
      C.unitConstraint.initWatchedLiterals must be equalTo ConstraintState.UNIT
    }
  }

  "After initializing the watched literals, unitConstraint" should {
    "be sat" in {
      C.satConstraint.initWatchedLiterals must be equalTo ConstraintState.SAT
    }
  }
}
