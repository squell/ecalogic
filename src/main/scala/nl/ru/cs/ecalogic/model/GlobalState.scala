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

/**
 * @author Marc Schoolderman
 * @author Jascha Neutelings
 */


case class GlobalState(gamma: GlobalState.States, t: Polynomial) {
  import GlobalState._

  /* Model the effect of calling fun on component */
  def update(component: String, fun: String): GlobalState = {
    val (state, t1) = gamma(component).update(fun, t)
    GlobalState(gamma.updated(component, state), t1)
  }

  /* Synchronize all components to the global timer; time passes */
  def sync: GlobalState = mapValues(_.forward(t))

  /* Synchronize all components to the global timer; no time passes */
  def timeshift: GlobalState = mapValues(st=>st.update(t, st.e))

  // Scala doesn't (and can't possibly?) know that the two EACStates are the same type.
  def max(other: GlobalState): GlobalState =
    GlobalState(gamma.transform((comp,st) => st.lub(other.gamma(comp))), t max other.t)

  /* pass-thru functions */
  def apply(name: String): ComponentModel#EACState = gamma(name)

  def transform[C](fun: (String,ComponentModel#EACState)=>C): (Map[String, C], Polynomial) =
    (gamma.transform(fun), t)

  def mapValues[C](fun: (ComponentModel#EACState=>C)): (Map[String, C], Polynomial) =
    (gamma.mapValues(fun), t)
}

object GlobalState {
  import scala.language.implicitConversions

  /** A map containing, for each component (identified by name), its current component state */
  type States = Map[String,ComponentModel#EACState]

  def initial(components: Map[String, ComponentModel]): GlobalState =
    GlobalState(components.mapValues(_.initialEACState()), 0)

  implicit def tupleToGlobalState(gamma: (States, Polynomial)): GlobalState = GlobalState(gamma._1, gamma._2)

}
