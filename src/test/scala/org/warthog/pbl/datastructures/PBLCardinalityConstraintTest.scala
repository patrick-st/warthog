package org.warthog.pbl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pbl.C
import scala.collection.mutable

import scala.collection.mutable.ListBuffer

/**
 * Tests for PBLCardinalityConstraints
 */
class PBLCardinalityConstraintTest extends Specification {
  sequential
  C.x1.state = State.OPEN
  C.x1_1.state = State.OPEN
  C.x2.state = State.OPEN
  C.x3.state = State.OPEN
  C.x4.state = State.OPEN
  C.x5.state = State.OPEN

  "c2" should {
    "be equal to c2" in {
      C.c2 must be equalTo C.c2
    }
    "be equal to c2_1" in {
      C.c2 must be equalTo C.c2_1
    }
    "not be equal to c2_2" in {
      C.c2 == C.c2_2 must be equalTo false
    }
    "not be equal to c2_3" in {
      C.c2 == C.c1_3 must be equalTo false
    }
    "not be equal to c1" in {
      C.c2 == C.c1 must be equalTo false
    }
  }

  "After initializing the watched literals, c2" should {
    "have the ConstraintState SUCCESS" in {
      val state = C.c2.initWatchedLiterals()
      state must be equalTo ConstraintState.UNRESOLVED
    }
    "watch literal l1" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l1)) must be equalTo true
    }
    "watch literal l2" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l2)) must be equalTo true
    }
    "watch literal l4" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l4)) must be equalTo true
    }
    "not watch literal l5" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l5)) must be equalTo false
    }
  }

  val units = mutable.HashSet[Constraint]()
  "After assigning x1 and x2 to false, c2" should {
    "be unit" in {
      C.x1.assign(false, units, 1, null)
      C.x2.assign(false, units, 1, null)
      units.contains(C.c2) must be equalTo true
    }
    "watch literal l2" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l2)) must be equalTo true
    }
    "watch literal l4" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l4)) must be equalTo true
    }
    "watch literal l5" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l5)) must be equalTo true
    }
    "not watch literal l1" in {
      C.c2.watchedLiterals.contains(new PBLTerm(1, C.l1)) must be equalTo false
    }
  }

  "After assigning x3 to false, c2" should {
    "be empty" in {
      C.x4.assign(false, units, 1, null).get must be equalTo C.c2
    }
  }

  "After initializing the watched literals, emptyCardinalityConstraint" should {
    "be empty" in {
      C.emptyCardinalityConstraint.initWatchedLiterals() must be equalTo ConstraintState.EMPTY
    }
  }

  "After initializing the watched literals, unitCardinalityConstraint" should {
    "be unit" in {
      C.unitCardinalityConstraint.initWatchedLiterals() must be equalTo ConstraintState.UNIT
    }
  }

  "After initializing the watched literals, unitCardinalityConstraint" should {
    "be sat" in {
      C.satCardinalityConstraint.initWatchedLiterals() must be equalTo ConstraintState.SAT
    }
  }

}
