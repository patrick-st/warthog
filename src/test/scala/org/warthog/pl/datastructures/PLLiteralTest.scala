/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.warthog.pl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pl.parsers.tptp._
import org.warthog.pl.datastructures.cnf.{ PLLiteral, ImmutablePLClause => Clause }

/**
  * Test for propositional literals
  */
class PLLiteralTest extends Specification {
  "Literal x" should {
    "be formula x" in {
      PLLiteral("x".pl).toFormula must be equalTo "x".pl
    }
  }

  "Literal ~x" should {
    "be formula ~x" in {
      PLLiteral("x", false).toFormula must be equalTo "~x".pl
    }
  }

  "Literal x negated" should {
    "be literal ~x" in {
      PLLiteral("x".pl).negate must be equalTo PLLiteral("x", false)
    }
  }

  "Literal ~x negated" should {
    "be literal x" in {
      PLLiteral("x", false).negate must be equalTo PLLiteral("x".pl)
    }
  }

  "x as literal" should {
    "be literal x" in {
      PLLiteral("x".pl) must be equalTo PLLiteral("x", true)
    }
  }

  "~x as literal" should {
    "be literal ~x" in {
      PLLiteral("~x".pl) must be equalTo PLLiteral("x", false)
    }
  }

  "integer -1 as literal" should {
    "be literal ~1" in {
      PLLiteral(-1) must be equalTo PLLiteral("1", false)
    }
  }

  "integer 1 as literal" should {
    "be literal 1" in {
      PLLiteral(1) must be equalTo PLLiteral("1", true)
    }
  }

  "integer -241 as literal" should {
    "be literal ~241" in {
      PLLiteral(-241) must be equalTo PLLiteral("241", false)
    }
  }

  "integer 432 as literal" should {
    "be literal 432" in {
      PLLiteral(432) must be equalTo PLLiteral("432", true)
    }
  }

  "integer 0 as literal" should {
    "should throw an exception" in {
      PLLiteral(0) must throwA[IllegalArgumentException]
    }
  }
}
