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

abstract class LinearComponentModel(val name: String) extends ComponentModel {

  case class CState(content: Map[String, ECAValue] = Map.empty, power: ECAValue = 0, tau: ECAValue = 0) extends ComponentState {
    val elements: Map[String, ECAValue] = content + ("power" -> power) + ("tau" -> tau)

    def this(elements: Map[String, ECAValue]) =
      this(elements -- Seq("power", "tau"), elements("power"), elements("tau"))

    protected def update(newElements: Map[String, ECAValue]): CState = CState(elements ++ newElements)

    def update(level: ECAValue, delay: ECAValue) = CState(content, level, tau+delay)

  }

  //override def td(s: EACState, t: ECAValue) = s.power * ((t - s.tau) max 0)

}

object DemoComponent extends LinearComponentModel("Demo") {

  val initialState = CState()

//  override def rc(fun: String)(gamma: CState, delta: GlobalState, args: Seq[ECAValue]): (CState, GlobalState) = {
//    fun match {
//      case "on"  => (gamma.update(1, 0), delta)
//      case "off" => (gamma.update(0, 0), delta)
//      case "idle"=> (gamma.update(gamma.power, 1), delta)
//      case _     => (gamma, delta)
//    }
//  }

}
