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

import scala.math.{ScalaNumericConversions, ScalaNumber}
import scala.collection.immutable.NumericRange

class ECAValue private(private val value: BigInt) extends ScalaNumber with ScalaNumericConversions with Ordered[ECAValue] {

  def isWhole = true

  def +(that: ECAValue) = new ECAValue(value + that.value)
  def -(that: ECAValue) = new ECAValue(value - that.value)
  def *(that: ECAValue) = new ECAValue(value * that.value)
  def /(that: ECAValue) = new ECAValue(value / that.value)
  def %(that: ECAValue) = new ECAValue(value % that.value)
  def ^(that: ECAValue) = new ECAValue(value ^ that.value)
  def unary_-           = new ECAValue(-value)
  def unary_+           = this

  def min(that: ECAValue) = if (this > that) that else this
  def max(that: ECAValue) = if (this > that) this else that

  def to(that: ECAValue) = NumericRange.inclusive[ECAValue](this, that, ECAValue.One)(ECAValue.ECAValueIsIntegral) // Can't find implicit? WHAT?

  def in(from: ECAValue, to: ECAValue) {
    require(this >= from && this <= to)

    // TODO: or not TODO?
  }

  def compare(that: ECAValue) = value compare that.value

  def &&(that: ECAValue) = ECAValue.booleanToValue(toBoolean && that.toBoolean)
  def ||(that: ECAValue) = ECAValue.booleanToValue(toBoolean || that.toBoolean)
  def unary_!            = ECAValue.booleanToValue(!toBoolean)

  def intValue    = value.intValue
  def longValue   = value.longValue
  def floatValue  = value.floatValue
  def doubleValue = value.doubleValue

  def underlying  = value

  def toBigInt  = value
  def toPolynomial = Polynomial(value)
  def toBoolean = value != ECAValue.False

  override def equals(that: Any) = that match {
    case that: ECAValue   => value == that.value
    case x                => value == x
  }

  override def hashCode = value.hashCode

  override def toString = value.toString

}

object ECAValue {
  import scala.language.implicitConversions

  val Zero = new ECAValue(0)
  val One  = new ECAValue(1)

  private val True  = One
  private val False = Zero

  implicit def valueToPoly(v: ECAValue): Polynomial = v.toPolynomial
  implicit def valueToBigInt(v: ECAValue): BigInt   = v.toBigInt
  implicit def valueToInt(v: ECAValue): Int         = v.toInt
  implicit def valueToBoolean(v: ECAValue): Boolean = v.toBoolean

  implicit def bigIntToValue(v: BigInt): ECAValue   = new ECAValue(v)
  implicit def intToValue(v: Int): ECAValue         = new ECAValue(v)
  implicit def booleanToValue(v: Boolean): ECAValue = if (v) True else False

  implicit object ECAValueIsIntegral extends Integral[ECAValue] with ECAValueOrdering  {
    def plus(x: ECAValue, y: ECAValue)  = x + y
    def minus(x: ECAValue, y: ECAValue) = x - y
    def times(x: ECAValue, y: ECAValue) = x * y
    def quot(x: ECAValue, y: ECAValue)  = x / y
    def rem(x: ECAValue, y: ECAValue)   = x % y
    def negate(x: ECAValue)             = -x
    def fromInt(x: Int)                 = intToValue(x)
    def toInt(x: ECAValue)              = x.toInt
    def toLong(x: ECAValue)             = x.toLong
    def toFloat(x: ECAValue)            = x.toFloat
    def toDouble(x: ECAValue)           = x.toDouble
  }

  trait ECAValueOrdering extends Ordering[ECAValue] {
    def compare(x: ECAValue, y: ECAValue) = x compare y
  }

  implicit object ECAValueOrdering extends ECAValueOrdering

}
