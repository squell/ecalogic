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

abstract class DSLModel(val name: String) extends ComponentModel with DelayedInit {

  type RCFunction  = (State, GlobalState, Seq[ECAValue]) => (State, GlobalState)
  type TDFunction  = (State, ECAValue) => ECAValue
  type LUBFunction = (State, State) => State
  type RFunction   = (State, State) => State

  protected class State(val elements: Map[String, TypedECAValue]) extends ComponentState with Dynamic {

    def selectDynamic(name: String) = elements(name)

    def applyDynamicNamed(name: String)(args: (String, ECAValue)*): State = name match {
      case "apply" =>
        val newElements = args.map {
          case (name, value) => name -> typeOfElement(name)(value)
        }.toMap
        val illegal = newElements.keySet &~ elements.keySet
        if (!illegal.isEmpty) {
          throw new ECAException(s"State transformer contains illegal elements: ${illegal.mkString(", ")}.")
        }
        new State(elements ++ newElements)
    }

    protected def update(newElements: Map[String, TypedECAValue]): State = new State(elements ++ newElements)

  }

  private var elements                 = Map[String, TypedECAValue]()
  private var constants                = Map[String, TypedECAValue]()
  private var rcFunctions              = Map[String, RCFunction]()
  private var tdFunction: TDFunction   = super.td
  private var lubFunction: LUBFunction = super.lub
  private var rFunction: RFunction     = super.r
  private var registering              = false

  private def checkRegistering() {
    if (!registering) {
      throw new IllegalStateException("Registration closed.")
    }
  }

  private def checkRegistrationClosed() {
    if (registering) {
      throw new IllegalStateException("Registration not yet finished.")
    }
  }

  private def checkDuplicates[A](seq: Seq[(String, A)], description: String, showDuplicateValues: Boolean) {
    val duplicates = seq.groupBy(_._1).filter(_._2.size > 1).mapValues(_.map(_._2))
    if (!duplicates.isEmpty) {
      val duplicatesString = duplicates.map {
        case (name, values) if showDuplicateValues => s"'$name' ${values.map(v => s"$name := $v").mkString("(", ", ", ")")}"
        case (name, _)                             => s"'$name'"
      }.mkString(",")

      throw new ECAException(s"One or more $description were redeclared: $duplicatesString.")
    }
  }

  def delayedInit(initCode: => Unit) = {
    registering = true
    initCode
    registering = false
  }

  protected def s0 = initialState

  lazy val initialState = {
    checkRegistrationClosed()
    new State(elements)
  }

  override def rc(fun: String)(s: State, d: GlobalState, a: Seq[ECAValue]) = rcFunctions(fun)(s, d, a)

  override def td(s: State, t: ECAValue) = tdFunction(s, t)

  override def lub(a: State, b: State) = lubFunction(a, b)

  override def r(s: State, old: State) = rFunction(s, old)

  protected object define {

    object s0 extends Dynamic {
      def applyDynamicNamed(name: String)(declarations: (String, ECAValue)*) {
        name match {
          case "apply" =>
            checkRegistering()
            checkDuplicates(declarations, "variables", true)
            elements = DSLModel.createTypedByNameMap(declarations.toMap).withDefault(n => throw new ECAException(s"Undeclared element: '$n'."))
        }
      }
    }

    object c extends Dynamic {
      def applyDynamicNamed(name: String)(declarations: (String, ECAValue)*) {
        name match {
          case "apply" =>
            checkRegistering()
            checkDuplicates(declarations, "constants", true)
            constants = declarations.toMap.mapValues(ECAInteger(_)).withDefault(n => throw new ECAException(s"Undeclared constant: '$n'."))
        }
      }
    }

    object rc extends Dynamic {
      def applyDynamicNamed(name: String)(declarations: (String, RCFunction)*) {
        name match {
          case "apply" =>
            checkRegistering()
            checkDuplicates(declarations, "resource consumption functions", false)
            rcFunctions = declarations.toMap.withDefault(DSLModel.super.rc)
        }
      }
    }

    def td(f: TDFunction) = {
      checkRegistering()
      tdFunction = f
    }

    def lub(f: LUBFunction) = {
      checkRegistering()
      lubFunction = f
    }

    def r(f: RFunction) = {
      checkRegistering()
      rFunction = f
    }

  }

  protected object c extends Dynamic {
    def selectDynamic(name: String) = {
      checkRegistrationClosed()
      constants(name)
    }
  }

}

object DSLModel {

  private def determineTypeByName(name: String) =
    """[\p{InGreek}$]""".r.findPrefixOf(name) match {
      case Some(_) => ECATimestamp
      case _       => ECAInteger
    }

  private def createTypedByNameMap(m: Map[String, ECAValue]) = m.map {
    case (name, value) => name -> determineTypeByName(name)(value)
  }

}



// Example DSL-based model of a radio.
// NOTE: Uses Unicode characters!
object Radio extends DSLModel("radio") {

  // Defines the initial state.
  // Variables starting with a Greek letter or a dollar sign ($) are considered timestamps.
  // NOTE: DSL limitation requires you to specify an initial value.
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
    on    = (s, Δ, _) => (s(τ = Δ.t, on = true)  , Δ(dT = +c.ton,        dE = c.eon        + td(s, Δ.t))),
    off   = (s, Δ, _) => (s(τ = Δ.t, on = 0)     , Δ(dT = +c.toff,       dE = c.eoff       + td(s, Δ.t))),
    queue = (s, Δ, _) => (s(τ = Δ.t, q = s.q + 1), Δ(dT = +c.tq,         dE = c._eq        + td(s, Δ.t))),
    send  = (s, Δ, _) => (s(τ = Δ.t, q = 0)      , Δ(dT = +c.tsendconst, dE = c.esendconst + td(s, Δ.t)))
  )

  // Overrides the td function.
  define td ((s, t) => if (t > s.τ) (t - s.τ) * (if (s.on) c.εon else c.εoff) else 0)

  // Overrides the lub function.
  // NOTE: The implementation below does exactly the same as the default implementation.
  //define lub ((s1, s2) => s0(on = s1.on max s2.on, q = s1.q max s2.q, τ = s1.τ min s2.τ))

  // Overrides the lub function.
  // NOTE: The implementation below does exactly the same as the default implementation.
  //define r ((s, sOld) => s(τ = sOld.τ))

}

object RadioTest extends App {

  val s0 = Radio.initialState
  val g0 = GlobalState.initial
  val transformers = Seq("on", "queue", "queue", "send", "off")

  val states = transformers.foldLeft(Seq((s0, g0))) {
    case (xs @ :+(_, (s, g)), t) => xs :+ Radio.rc(t)(s, g, Seq.empty)
  }

  val padding = transformers.map(_.length).max + 7

  val message = (None +: transformers.map(Some(_))).zip(states).map {
    case (None   , (s, g)) =>                 Seq.fill(padding)(' ').mkString + s"[$s, $g]"
    case (Some(t), (s, g)) => s"==[$t]=>\n" + Seq.fill(padding)(' ').mkString + s"[$s, $g]"
  }.mkString("\n")

  println(message)

}

