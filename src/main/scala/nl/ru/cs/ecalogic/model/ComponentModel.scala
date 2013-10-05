
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

package nl.ru.cs.ecalogic.model

import scala.collection.immutable.Map

trait ComponentModel {

  type State <: ComponentState

  protected trait ComponentState {
//    protected val elements: Map[String, Value]

//    private[ComponentModel] lazy val timestamps = elements.collect {
//      case (k, Timestamp(v)) => (k, v)
//    }
//
//    private[ComponentModel] lazy val variables = elements.collect {
//      case (k, Integer(v)) => (k, v)
//    }

    val timestamps: Map[String, BigInt]
    val variables: Map[String, BigInt]

    override def toString = s"[ variables: ${variables.mkString(", ")}| timestamps: ${timestamps.mkString(", ")}]"
  }

  //protected def newState(overrides: Map[String, Value]): State

  def newState: State = newState(Map.empty, Map.empty)

  def newState(integers: Map[String, BigInt], timestamps: Map[String, BigInt]): State

  def lub(a: State, b: State) =
    newState(a.variables.keys.map(key => key->(a.variables(key) max b.variables(key))).toMap,
             a.timestamps.keys.map(key => key->(a.timestamps(key) min b.timestamps(key))).toMap)

  def lteq(a: State, b: State) =
    a.variables.keys.forall(key => a.variables(key) <= b.variables(key)) &&
    a.timestamps.keys.forall(key => a.timestamps(key) >= b.timestamps(key))

  def tryCompare(a: State, b: State): Option[Int] = {
    if(lteq(a,b)) {
      if(lteq(b,a)) Some(0)
      else Some(-1)
    } else if (lteq(b,a))
      Some(+1)
    else
      None
  }

  def r(s: State, old: State) =
    newState(s.variables, old.timestamps)

  def td(s: State, t: BigInt) = 0

  // Does not actually operate on the set of component states, but only the component state that belongs to this model.
  // Might be sufficient?
  def rc(f: String, s: State, g: GlobalState, args: Seq[BigInt]): (State, GlobalState) = (s, g)

}

case class GlobalState(time: BigInt, energy: BigInt)

//sealed trait Value extends ScalaNumber with ScalaNumericConversions {
//
//  type T <: Value
//
//  def value: BigInt
//
//  protected def newValue(v: BigInt): T
//
//  def isWhole() = true
//
//  def +(that: T): T = newValue(value + that.value)
//
//  def -(that: T): T = newValue(value - that.value)
//
//  def *(that: T): T = newValue(value * that.value)
//
//  def intValue() = value.intValue()
//
//  def longValue() = value.longValue()
//
//  def floatValue() = value.floatValue()
//
//  def doubleValue() = value.doubleValue()
//
//  def underlying(): BigInt = value
//
//  def toTimestamp = Timestamp(value)
//
//  def toInteger = Integer(value)
//
//}
//
//case class Integer(value: BigInt = 0) extends Value with Ordered[Integer] {
//
//  type T = Integer
//
//  protected def newValue(v: BigInt) = Integer(v)
//
//  def compare(that: Integer): Int = value.compare(that.value)
//
//  def and(that: Integer): Integer = newValue(if (value != BigInt(0) && that.value != BigInt(0)) BigInt(1) else BigInt(0))
//
//  def or(that: Integer): Integer = newValue(if (value != BigInt(0) || that.value != BigInt(0)) BigInt(1) else BigInt(0))
//
//  override def toInteger = this
//
//}
//
//case class Timestamp(value: BigInt = 0) extends Value with Ordered[Timestamp] {
//
//  type T = Timestamp
//
//  def compare(that: Timestamp): Int = value.compare(that.value)
//
//  protected def newValue(v: BigInt) = Timestamp(v)
//
//  override def toTimestamp = this
//
//}
