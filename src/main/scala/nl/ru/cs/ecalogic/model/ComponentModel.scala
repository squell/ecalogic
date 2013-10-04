
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
import scala.collection.immutable.HashMap

trait ComponentModel {

  trait ComponentState extends PartialFunction[String, Variable] with PartiallyOrdered[ComponentState] {
    def timestamps: HashMap[String,Int]
    def integers: HashMap[String,Int]

    def follows(that: ComponentState) = {
      val vr_GE = integers.  keys.forall(key => integers  (key) >= that.integers  (key))
      val ts_LE = timestamps.keys.forall(key => timestamps(key) <= that.timestamps(key))
      ts_LE && vr_GE
    }

    def tryCompareTo(that: ComponentState) = {
      if(follows(that)) {
        if(that.follows(this)) Some(0)
        else Some(1)
      } else if (that.follows(this)) {
        Some(-1)
      } else {
        None
      }
    }
  }

//  def lub(x:St, y: St) = {
//    x.vars  y.vars
//    x.timestamps y.timestmaps
//  }
//
//
//  A,B: Map[String,Int]
//   Map[String,Int]
//  - voor elke waarde 'x'
//     C['x'] := f(A['x'], B['x'])

  def symbol: Seq[Symbol]

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
