package org.warthog.pbl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pbl.C

/**
 * Test for PBLVariable
 */
class PBLVariableTest extends Specification{

  "x1" should {
    "be equal to x1" in {
      C.x1 == C.x1 must be equalTo true
    }
    "be equal to x1_1" in {
      C.x1 == C.x1_1 must be equalTo true
    }
    "not be equal to x2" in {
      C.x1 == C.x2 must be equalTo false
    }
  }
}
