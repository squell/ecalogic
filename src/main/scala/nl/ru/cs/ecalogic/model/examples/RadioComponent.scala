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

  define s0 (
    on = 0
  )

  define T (
    on   = 40,
    off  = 20,
    q    = 3 ,
    send = 100
  )

  define E (
    on   = 40,
    off  = 20,
    q    = 3 ,
    send = 100
  )

  define δ (
    on  = s => s(on = true),
    off = s => s(on = false)
  )

  define ϕ (s => 2 + 100 * s.on)

}

//object RadioComponentTest extends App {
//
//  val s0 = RadioComponent.initialState
//  val g0 = GlobalState.initial
//  val transformers = Seq("on", "queue", "queue", "send", "off")
//
//  val states = transformers.foldLeft(Seq((s0, g0))) {
//    case (xs @ :+(_, (s, g)), t) => xs :+ RadioComponent.rc(t)(s, g, Seq.empty)
//  }
//
//  val padding = transformers.map(_.length).max + 7
//
//  val message = (None +: transformers.map(Some.apply)).zip(states).map {
//    case (None   , (s, g)) =>                 Seq.fill(padding)(' ').mkString + s"[$s, $g]"
//    case (Some(t), (s, g)) => s"==[$t]=>\n" + Seq.fill(padding)(' ').mkString + s"[$s, $g]"
//  }.mkString("\n")
//
//  println(message)
//
//}
