
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

class ComponentState(val time: Map[String,Int], val ints: Map[String,Int]) //extends PartiallyOrdered[ComponentState] {
//class ComponentState(val time: Map[String,Int], val ints: Map[String,Int]) extends PartialFunction[String, Variable] with PartiallyOrdered[ComponentState] {

  // should we really implement PartialFunction?

/*

  def tryCompareTo(that: ComponentState): Option[Int] = {
    if(follows(that)) {
      if(that.follows(this)) Some(0)
      else Some(1)
    } else if (that.follows(this)) 
      Some(-1)
    else
      None
  }
  */


trait ComponentModel extends PartialOrdering[ComponentState] {

  def lub(a: ComponentState, b: ComponentState) = {
    new ComponentState(a.ints.keys.map(key => key->(a.ints(key) max b.ints(key))).toMap,
		       a.time.keys.map(key => key->(a.time(key) min b.time(key))).toMap)
  }

  def lteq(a: ComponentState, b:ComponentState) = 
    a.ints.keys.forall(key => a.ints(key) <= b.ints(key)) &&
    a.time.keys.forall(key => a.time(key) >= b.time(key))

  def tryCompareTo(a: ComponentState, b:ComponentState): Option[Int] = {
    if(lteq(a,b)) {
      if(lteq(b,a)) Some(0)
      else Some(-1)
    } else if (lteq(b,a))
      Some(+1)
    else
      None
  }

  def r(s: ComponentState, old: ComponentState) =
    new ComponentState(s.ints, old.time)

  def td(s: ComponentState, t: Int) = 0

// todo: rcf

}

sealed trait Binding {
  def name: String
  def typ: Type
  def value: BigInt
}

case class Variable(name: String, typ: Type, value: BigInt) extends Binding
case class Constant(name: String,            value: BigInt) extends Binding {
  def typ = Integer
}

sealed trait Type {
  def defaultValue: BigInt
}

case object Integer extends Type {
  val defaultValue: BigInt = 0
}

case object Timestamp extends Type  {
  val defaultValue: BigInt = 0
}
