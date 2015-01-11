package org.warthog.pbl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pbl.C

/**
 * Test for PBLTerm
 */
class PBLTermTest extends Specification {

  "t1" should {
    "be equal to t1" in {
      C.t1 must be equalTo C.t1
    }
    "be equal to t1_1" in {
      C.t1 must be equalTo C.t1_1
    }
    "not be equal to t1_2" in {
      C.t1 == C.t1_2 must be equalTo false
    }
    "not be equal to t1_3" in {
      C.t1 == C.t1_3 must be equalTo false
    }
    "not be equal to t2" in {
      C.t1 == C.t2 must be equalTo false
    }
  }

}
