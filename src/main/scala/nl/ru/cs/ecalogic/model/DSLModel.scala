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

import scala.collection.mutable

abstract class DSLModel(val name: String) extends ComponentModel with DelayedInit {

  type RCFunction  = (State, GlobalState, Seq[ECAValue]) => (State, GlobalState)
  type TDFunction  = (State, ECAValue) => ECAValue
  type LUBFunction = (State, State) => State
  type RFunction   = (State, State) => State

  class State private[DSLModel](val elements: Map[String, ECAValue]) extends ComponentState with Dynamic {
    import scala.language.dynamics

    def selectDynamic(name: String) = elements(name)

    def applyDynamicNamed(name: String)(args: (String, ECAValue)*): State = name match {
      case "apply" =>
        val newElements = args.toMap
        val undefined = newElements.keySet &~ elements.keySet
        if (!undefined.isEmpty) {
          throw new ECAException(s"State transformer contains undefined elements: ${undefined.mkString(", ")}.")
        }
        new State(elements ++ newElements)
    }

    protected def update(newElements: Map[String, ECAValue]) = new State(elements ++ newElements)

  }

  private val elements                 = mutable.Map.empty[String, ECAValue].withDefault(n => throw new ECAException(s"Undefined element: '$n'."))
  private val constants                = mutable.Map.empty[String, ECAValue].withDefault(n => throw new ECAException(s"Undefined constant: '$n'."))
  private val rcFunctions              = mutable.Map.empty[String, RCFunction].withDefault(DSLModel.super.rc)
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
      throw new IllegalStateException("Registration in progress.")
    }
  }

  private def checkDuplicates[A](seq: Seq[(String, A)], description: String, showDuplicateDefinitions: Boolean) {
    val duplicates = seq.groupBy(_._1).filter(_._2.size > 1).mapValues(_.map(_._2))
    if (!duplicates.isEmpty) {
      val duplicatesString = duplicates.map {
        case (name, values) if showDuplicateDefinitions =>
          s"'$name' ${values.map(v => s"$name = $v").mkString("(", ", ", ")")}"
        case (name, _) =>
          s"'$name'"
      }.mkString(", ")

      throw new ECAException(s"One or more $description were redefined: $duplicatesString.")
    }
  }

  def delayedInit(initCode: => Unit) = {
    registering = true
    initCode
    registering = false
  }

  def s0 = initialState

  lazy val initialState = {
    checkRegistrationClosed()
    new State(elements.toMap)
  }

  protected def isTimestamp(name: String) = TimestampCache.isTimestamp(name)

  override def rc(fun: String)(s: State, d: GlobalState, a: Seq[ECAValue]) = rcFunctions(fun)(s, d, a)

  override def td(s: State, t: ECAValue) = tdFunction(s, t)

  override def lub(a: State, b: State) = lubFunction(a, b)

  override def r(s: State, old: State) = rFunction(s, old)

  protected object define {

    private[define] abstract class Declaration[T] extends Dynamic {
      import scala.language.dynamics

      def applyDynamicNamed(name: String)(declarations: (String, T)*) {
        name match {
          case "apply" =>
            checkRegistering()
            apply(declarations)
          case name =>
            throw new RuntimeException(s"Undeclared method: $name.")
        }
      }

      def applyDynamic(name: String)() {
        applyDynamicNamed(name)()
      }

      protected def apply(declarations: Seq[(String, T)])

    }

    object s0 extends Declaration[ECAValue] {

      protected def apply(declarations: Seq[(String, ECAValue)]) {
        checkDuplicates(declarations, "variables", true)
        elements ++= declarations.toMap
      }

    }

    object c extends Declaration[ECAValue] {

      protected def apply(declarations: Seq[(String, ECAValue)]) {
        checkDuplicates(declarations, "constants", true)
        constants ++= declarations.toMap
      }

    }

    object rc extends Declaration[RCFunction] {

      protected def apply(declarations: Seq[(String, RCFunction)]) {
        checkDuplicates(declarations, "resource consumption functions", false)
        rcFunctions ++= declarations.toMap
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

  object c extends Dynamic {
    import scala.language.dynamics

    def selectDynamic(name: String) = {
      checkRegistrationClosed()
      constants(name)
    }

  }

  private object TimestampCache {

    private val timestampPattern = """[\p{InGreek}$]""".r

    private val cache = mutable.Map.empty[String, Boolean]

    def isTimestamp(name: String) = cache.getOrElseUpdate(name, timestampPattern.findPrefixOf(name).isDefined)

  }

}



