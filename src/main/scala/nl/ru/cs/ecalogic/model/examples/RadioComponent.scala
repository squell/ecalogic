/*
 * ecalogic: a tool for performing energy consumption analysis.
 *
 * Copyright (c) 2013, J. Neutelings, D. Peelen, M. Schoolderman
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
package model
package examples

// Example DSL-based model of a radio.
// NOTE: Uses Unicode characters!
object RadioComponent extends DSLModel("radio") {

  // Defines the initial state.
  // Variables starting with a Greek letter or a dollar sign ($) are considered timestamps.
  // NOTE: DSL limitation requires specifying an initial value.
  define s0 (
    on = 0,
    q  = 0,
    τ  = 0
  )

  // Defines the constants.
  // The values are always of type integer.
  // NOTE: You can also use Scala-style constants (val), but this structure was added for consistency.
  define c (
    ton        = 40,
    toff       = 20,
    tq         = 3 ,
    tsendconst = 10,
    tsendq     = 1 ,
    eon        = 40,
    eoff       = 20,
    _eq        = 3 ,
    esendconst = 10,
    esendq     = 3 ,
    εon        = 30,
    εoff       = 2
  )

  // Defines the resource consumption functions.
  define rc (
    on    = (s, Δ, _) => (s(τ = Δ.t, on = true)  , (Δ.t + c.ton       , Δ.e + c.eon        + td(s, Δ.t))),
    off   = (s, Δ, _) => (s(τ = Δ.t, on = 0)     , (Δ.t + c.toff      , Δ.e + c.eoff       + td(s, Δ.t))),
    queue = (s, Δ, _) => (s(τ = Δ.t, q = s.q + 1), (Δ.t + c.tq        , Δ.e + c._eq        + td(s, Δ.t))),
    send  = (s, Δ, _) => (s(τ = Δ.t, q = 0)      , (Δ.t + c.tsendconst, Δ.e + c.esendconst + td(s, Δ.t)))
  )

  // Overrides the td function.
  define td ((s, t) => if (t > s.τ) (t - s.τ) * (if (s.on) c.εon else c.εoff) else 0)

  // Overrides the lub function.
  // NOTE: The implementation below does exactly the same as the default implementation.
  define lub ((s1, s2) => s0(on = s1.on max s2.on, q = s1.q max s2.q, τ = s1.τ min s2.τ))

  // Overrides the r function.
  // NOTE: The implementation below does exactly the same as the default implementation.
  define r ((s, sOld) => s(τ = sOld.τ))

}

object RadioComponentTest extends App {

  val s0 = RadioComponent.initialState
  val g0 = GlobalState.initial
  val transformers = Seq("on", "queue", "queue", "send", "off")

  val states = transformers.foldLeft(Seq((s0, g0))) {
    case (xs @ :+(_, (s, g)), t) => xs :+ RadioComponent.rc(t)(s, g, Seq.empty)
  }

  val padding = transformers.map(_.length).max + 7

  val message = (None +: transformers.map(Some.apply)).zip(states).map {
    case (None   , (s, g)) =>                 Seq.fill(padding)(' ').mkString + s"[$s, $g]"
    case (Some(t), (s, g)) => s"==[$t]=>\n" + Seq.fill(padding)(' ').mkString + s"[$s, $g]"
  }.mkString("\n")

  println(message)

}
