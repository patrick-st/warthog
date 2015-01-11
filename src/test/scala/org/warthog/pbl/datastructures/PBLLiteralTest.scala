package org.warthog.pbl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pbl.C

/**
 * Test for PBLLiteral
 */
class PBLLiteralTest extends Specification {
  C.x1.state = State.TRUE
  C.x2.state = State.FALSE
  C.x3.state = State.TRUE
  C.x4.state = State.FALSE

  "l1" should {
    "be equal to l1" in {
      C.l1 == C.l1 must be equalTo true
    }
    "be equal to l1_1" in {
      C.l1 == C.l1_1 must be equalTo true
    }
    "not be equal to l2" in {
      C.l1 == C.l2 must be equalTo false
    }
    "evaluate to true" in {
      C.l1.evaluates2True must be equalTo true
    }
  }

  "l2" should {
    "evaluate to false" in {
      C.l2.evaluates2True must be equalTo false
    }
  }

  "l3_" should {
    "evaluate to false" in {
      C.l3_.evaluates2True must be equalTo false
    }
  }

  "l4_" should {
    "evaluate to true" in {
      C.l4_.evaluates2True must be equalTo true
    }
  }

  "l5" should {
    "evaluate to false" in {
      C.l5.evaluates2True must be equalTo false
    }
    "be negated to ~l5" in {
      C.l5.negate must be equalTo new PBLLiteral(C.x5, false)
    }
    "be double negated to l5" in {
      C.l5.negate.negate must be equalTo C.l5
    }
  }


}
