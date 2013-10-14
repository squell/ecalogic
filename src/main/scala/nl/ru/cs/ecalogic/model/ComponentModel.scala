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

/**
 * @author Marc Schoolderman
 * @author Jascha Neutelings
 */
trait ComponentModel {

  type CState <: ComponentState

  trait ComponentState extends PartiallyOrdered[CState] {

    val elements: Map[String, ECAValue]

    def tryCompareTo[B >: CState <% PartiallyOrdered[B]](that: B): Option[Int] = that match {
      case that: ComponentState =>
        var sign = 0
        for (key <- elements.keys) {
          val cmp = elements(key) compare that.elements(key)
          if (sign* cmp < 0) return None // note: a*b<0 iff a,b have different signs
          else sign |= cmp
        }
//        for (key <- timestamps.keys) {
//          val cmp = -(timestamps(key) compare that.timestamps(key))
//          if (sign *cmp < 0) return None
//          else sign |= cmp
//        }
        Some(sign)
      case _ => None
    }

    protected def update(newElements: Map[String, ECAValue]): CState

    private[ComponentModel] def _update(newElements: Map[String, ECAValue]) = update(newElements)

    override def equals(that: Any) = that match {
      case that: ComponentState => tryCompareTo(that) == Some(0)
      case _                    => false
    }

    override def hashCode = elements.hashCode

    override def toString = elements.map {
      case (name, value) => s"$name = $value"
    }.mkString("(", ", ", ")")

  }

  case class EACState(state: CState, timestamp: ECAValue, energy: ECAValue) {
    def s = state
    def t = timestamp
    def e = energy
  }

  val name: String

  val initialState: CState

  def E(f: String) = ECAValue.Zero

  def T(f: String) = ECAValue.Zero

  def lub(a: EACState, b: EACState): EACState = {
    val EACState(sa, ta, ea) = a
    val EACState(sb, tb, eb) = b

    EACState(
      sa._update(sa.elements.keys.map(key => key -> (sa.elements(key) max sb.elements(key))).toMap),
      ta min tb,
      ea max eb
    )
  }

  def td(g: EACState, t: ECAValue): ECAValue = if (t > g.t) (t - g.t) * phi(g.s) else ECAValue.Zero

  def delta(f: String)(s: CState) = s

  def phi(s: CState) = ECAValue.Zero

}

trait CPUComponent { this: ComponentModel => }
