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

import util.Polynomial

import scala.collection.mutable

abstract class DSLModel(val name: String) extends ComponentModel with DelayedInit {

  class CState private[DSLModel](val elements: Map[String, Polynomial]) extends ComponentState with Dynamic {
    import scala.language.dynamics

    def selectDynamic(name: String) = elements(name)

    def applyDynamicNamed(name: String)(args: (String, Polynomial)*): CState = name match {
      case "apply" =>
        val newElements = args.toMap
        val undefined = newElements.keySet &~ elements.keySet
        if (!undefined.isEmpty) {
          throw new ECAException(s"State transformer contains undefined elements: ${undefined.mkString(", ")}.")
        }
        new CState(elements ++ newElements)
      case name =>
        throw new RuntimeException(s"Undeclared method: $name.")
    }

    protected def update(newElements: Map[String, Polynomial]) = new CState(elements ++ newElements)

  }

  private val elements                 = mutable.Map.empty[String, Polynomial].withDefault(n => throw new ECAException(s"Undefined element: '$n'.")) // moet dit niet 0 zijn?
  private val energyConstants          = mutable.Map.empty[String, ECAValue].withDefault(super.E)
  private val timeConstants            = mutable.Map.empty[String, ECAValue].withDefault(super.T)
  private val deltaFunctions           = mutable.Map.empty[String, DFunction].withDefault(super.delta)
  private val rvFunctions              = mutable.Map.empty[String, RVFunction].withDefault(super.rv)
  private var tdFunction: TDFunction   = super.td
  private var lubFunction: LUBFunction = super.lub
  private var phiFunction: PHIFunction = super.phi
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
    new CState(elements.toMap)
  }

  override def E(f: String) = energyConstants(f)

  override def T(f: String) = timeConstants(f)

  override def delta(f: String)(s: CState) = deltaFunctions(f)(s)

  override def rv(f: String)(s: CState, a: Seq[ECAValue]) = rvFunctions(f)(s, a)

  override def td(s: EACState, t: Polynomial) = tdFunction(s, t)

  override def lub(a: EACState, b: EACState) = lubFunction(a, b)

  override def phi(s: CState) = phiFunction(s)

  override def functionArity(f: String) = if ((rvFunctions.keySet union deltaFunctions.keySet).contains(f)) Some(-1) else None

  override def hasFunctionInfo = true

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

    private[DSLModel] class VariableDeclaration(val value: ECAValue)

    val σ = s0
    object s0 extends Declaration[Polynomial] {

      protected def apply(declarations: Seq[(String, Polynomial)]) {
        checkDuplicates(declarations, "variables", true)
        elements ++= declarations.toMap
      }

    }

    object E extends Declaration[ECAValue] {

      protected def apply(declarations: Seq[(String, ECAValue)]) {
        checkDuplicates(declarations, "incidental energy constants", true)
        energyConstants ++= declarations.toMap
      }

    }

    object T extends Declaration[ECAValue] {

      protected def apply(declarations: Seq[(String, ECAValue)]) {
        checkDuplicates(declarations, "time cost constants", true)
        timeConstants ++= declarations.toMap
      }

    }

    val δ     = d
    val delta = d
    object d extends Declaration[DFunction] {

      protected def apply(declarations: Seq[(String, DFunction)]) {
        checkDuplicates(declarations, "state update functions", false)
        deltaFunctions ++= declarations.toMap
      }

    }

    object rv extends Declaration[RVFunction] {

      protected def apply(declarations: Seq[(String, RVFunction)]) {
        checkDuplicates(declarations, "result value functions", false)
        rvFunctions ++= declarations.toMap
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

    val ϕ = phi _
    def phi(f: PHIFunction) = {
      checkRegistering()
      phiFunction = f
    }

  }

//  object E extends Dynamic {
//    import scala.language.dynamics
//
//    def selectDynamic(name: String) = {
//      checkRegistrationClosed()
//      constants(name)
//    }
//
//  }

}



