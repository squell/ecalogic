/*
 * ecalogic: A tool for performing energy consumption analysis.
 *
 * Copyright (C) 2013, J. Neutelings, D. Peelen, M. Schoolderman
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 *   Neither the name of the Radboud University Nijmegen nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package nl.ru.cs.ecalogic
package test

import util.Polynomial

import org.scalatest.{FlatSpec, Matchers}

class PolynomialSpec extends FlatSpec with Matchers {

  behavior of "The Polynomial class"

  val zero = Polynomial(0)
  val one  = Polynomial(1)
  val x    = Polynomial("x")
  val y    = Polynomial("y")
  val z    = Polynomial("z")
  val A    = 5*x + 42*y
  val B    = 37*x + 23*z
  lazy val C = A*B
  val D    = 77*y + 7*z

  it should "have equality" in {
    A should equal(A)
    A should not equal(B)
  }

  it should "not throw exceptions" in {
    noException should be thrownBy (A+B)
    noException should be thrownBy (A*B)
  }

  it should "have neutral elements" in {
    A + zero should equal(A)
    zero + A should equal(A)
    A * one  should equal(A)
    one * A  should equal(A)
  }

  it should "have commutative operations" in {
    A + B should equal(B + A)
    A * B should equal(B * A)
  }

  it should "have a negative element" in {
    A - A should equal(zero)
    B - B should equal(zero)
    A + (B-A) should equal(B)
    B + (A-B) should equal(A)
  }

  it should "have associative operations" in {
    A+(B+C) should equal ((A+B) + C)
  }

  it should "distribute * over +" in {
    A * (B+C) should equal (A*B + A*C)
  }

  it should "turn double negation into +" in {
    A-(B-C) should equal ((A-B)+C)
  }

  it should "have true maximums and minimums" in {
    A <= (A max B) should be (true)
    A >  (A max B) should not be (true)
    A >= (A min B) should be (true)
    A <  (A min B) should not be (true)
  }

  it should "have commutative min and max" in {
    A max B should be (B max A)
    A min B should be (B min A)
  }

  it should "have associative min and max" in {
    A max (B max D) should be ((A max B) max D)
    A min (B min D) should be ((A min B) min D)
  }

  it should "distribute min and max" in {
    A min (B max D) should be ((A min B) max (A min D))
    A max (B min D) should be ((A max B) min (A max D))
    A + (B max D) should be ((A + B) max (A + D))
    A + (B min D) should be ((A + B) min (A + D))
    // note: does not distribute over *
  }

  it should "evaluate correctly" in {
    val env = Seq("x"->2, "y"->3, "z"->5)
    C(env:_*) + D(env:_*) should be ((C+D)(env:_*))
    C(env:_*) * D(env:_*) should be ((C*D)(env:_*))
    C("x"->0,"y"->0,"z"->0) should be (C.coef(Seq()))
  }
  it should "compose substitution" in {
    C("x"->A).apply("y"->B) should be (C("x"->A("y"->B), "y"->B))
  }
}

