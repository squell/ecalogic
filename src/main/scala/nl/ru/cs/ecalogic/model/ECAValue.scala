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

import scala.math.{ScalaNumericConversions, ScalaNumber}

class ECAValue(private val value: BigInt) extends ScalaNumber with ScalaNumericConversions with Ordered[ECAValue] {

  def isWhole = true

  def +(that: ECAValue) = new ECAValue(value + that.value)
  def -(that: ECAValue) = new ECAValue(value - that.value)
  def *(that: ECAValue) = new ECAValue(value * that.value)
  def /(that: ECAValue) = new ECAValue(value / that.value)
  def %(that: ECAValue) = new ECAValue(value % that.value)
  def unary_-           = new ECAValue(-value)
  def unary_+           = this

  def min(that: ECAValue) = if (this > that) that else this
  def max(that: ECAValue) = if (this > that) this else that

  def compare(that: ECAValue) = value.compare(that.value)

  def &&(that: ECAValue) = ECAValue.booleanToValue(toBoolean && that.toBoolean)
  def ||(that: ECAValue) = ECAValue.booleanToValue(toBoolean || that.toBoolean)
  def unary_!            = ECAValue.booleanToValue(!toBoolean)

  def intValue    = value.intValue
  def longValue   = value.longValue
  def floatValue  = value.floatValue
  def doubleValue = value.doubleValue

  def underlying  = value

  def toBigInt            = value
  def toBoolean           = value != ECAValue.False

  override def equals(that: Any) = that match {
    case that: ECAValue   => value == that.value
    case x                => value == x
  }

  override def hashCode = value.hashCode

  override def toString = value.toString

}

object ECAValue {

  val Zero = new ECAValue(0)
  val One  = new ECAValue(1)

  private val True  = One
  private val False = Zero

  implicit def valueToBigInt(v: ECAValue): BigInt   = v.toBigInt
  implicit def valueToInt(v: ECAValue): Int         = v.toInt
  implicit def valueToBoolean(v: ECAValue): Boolean = v.toBoolean

  implicit def bigIntToValue(v: BigInt): ECAValue   = new ECAValue(v)
  implicit def intToValue(v: Int): ECAValue         = new ECAValue(v)
  implicit def booleanToValue(v: Boolean): ECAValue = if (v) True else False

}

class TypedECAValue(value: BigInt, val typ: ECAType) extends ECAValue(value)

sealed trait ECAType {
  def apply(value: ECAValue) = new TypedECAValue(value.toBigInt, this)
  def unapply(value: TypedECAValue): Option[TypedECAValue] = if (value.typ == this) Some(value) else None
}

case object ECAInteger extends ECAType
case object ECATimestamp extends ECAType
