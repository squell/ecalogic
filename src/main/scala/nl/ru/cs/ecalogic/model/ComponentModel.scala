
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

case class GlobalState(time: BigInt, energy: BigInt) {
  def update(t: BigInt, e: BigInt) = new GlobalState(time+t, energy+e)
}

/*

  COMMENT/IDEA:

     we could use reflection to implement ComponentStates, e.g.:
     trait ComponentState
     class MyComponentState extends ComponentState {
       var x: IntType
       var t: TimeStamp
     }

     howwwwwwwwever. it seems to be a recent addition to the language, so perhaps
     it is risky to use this 'in production'?

 */

// isn't it useful to keep as a base class in a place we can access it?

class ComponentState(val timestamps: Map[String, BigInt], val variables: Map[String, BigInt]) {
//    protected val elements: Map[String, Value]

//    private[ComponentModel] lazy val timestamps = elements.collect {
//      case (k, Timestamp(v)) => (k, v)
//    }
//
//    private[ComponentModel] lazy val variables = elements.collect {
//      case (k, Integer(v)) => (k, v)
//    }

  def updateVariable(name: String, value: BigInt) =
    new ComponentState(timestamps, variables.updated(name,value))

  def updateTimestamp(name: String, value: BigInt) =
    new ComponentState(timestamps.updated(name,value), variables)

  override def toString = s"[ variables: ${variables.mkString(", ")}| timestamps: ${timestamps.mkString(", ")}]"
}

abstract class ComponentModel(val name: String) {

  type State <: ComponentState

  def newState: State = newState(Map.empty, Map.empty)

  def newState(integers: Map[String, BigInt], timestamps: Map[String, BigInt]): State 
    
  def lub(a: State, b: State) =
    newState(a.variables.keys.map (key => key->(a.variables (key) max b.variables (key))).toMap,
             a.timestamps.keys.map(key => key->(a.timestamps(key) min b.timestamps(key))).toMap)

  def lteq(a: State, b: State) =
    a.variables. keys.forall(key => a.variables (key) <= b.variables (key)) &&
    a.timestamps.keys.forall(key => a.timestamps(key) >= b.timestamps(key))

  def tryCompare(a: State, b: State): Option[Int] = {
    var sign = 0;
    for(key <- a.variables.keys) {
      val cmp = a.variables(key) compare b.variables(key)
      if(sign*cmp < 0) return None // note: a*b<0 iff a,b have different signs
      else sign |= cmp
    }
    for(key <- a.timestamps.keys) {
      val cmp = -(a.timestamps(key) compare b.timestamps(key))
      if(sign*cmp < 0) return None
      else sign |= cmp
    }
    return Some(sign)
  }

  def r(s: State, old: State) =
    newState(s.variables, old.timestamps)

  def td(s: State, t: BigInt): BigInt = 0

// humorous: convergent evolution in program design:

/* 
MARC:
  //according to the paper, this should be the signature:
  //def rc(fun: String, gamma: Set[ComponentState], delta: GlobalState) =

  //however, i currently fail to see why this is necessary
JASCHA:
  // Does not actually operate on the set of component states, but only the component state that belongs to this model.
  // Might be sufficient?
*/

  def rc(fun: String)(gamma: State, delta: GlobalState, args: Seq[BigInt]): (State, GlobalState) = (gamma, delta)

}

case class LinearComponentState(content: Map[String,BigInt], power:BigInt=0, tau:BigInt=0) extends ComponentState(Map("tau"->tau), content) {
  def update(level: BigInt, delay: BigInt) = 
    new LinearComponentState(content, level, tau+delay)
}

abstract class LinearComponentModel(name: String) extends ComponentModel(name) {

  override type State = LinearComponentState

  override def td(s: State, t: BigInt) = {
    s.power * ((t - s.tau) max 0)
  }

}

class DemoComponent extends LinearComponentModel("Demo") {

  override def newState(integers: Map[String,BigInt], timestamps: Map[String,BigInt]) =
    new LinearComponentState(Map.empty, 0, 0)

  override def rc(fun: String)(gamma: State, delta: GlobalState, args: Seq[BigInt]): (State, GlobalState) = {
    fun match {
      case "on"  => (gamma.update(1,0), delta)
      case "off" => (gamma.update(0,0), delta)
      case "idle"=> (gamma.update(gamma.power,1), delta)
      case _     => (gamma, delta)
    }
  }

}

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
