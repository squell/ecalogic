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
package util

/**
 * A simple class representing Polynomials
 *
 * class invariant:
 * - all Seq's used in the keys of repr must be sorted
 * - repr has a defaultValue of 0
 *
 * @author Marc Schoolderman
 */
class Polynomial private (private val repr: Map[Seq[String],BigInt]) extends PartiallyOrdered[Polynomial] {

  import Polynomial._

  def +(that: Polynomial) = new Polynomial(combine(this.repr, that.repr))

  def unary_- = -1*this
  def -(that: Polynomial) = this + -that

  def *(that: Polynomial) = new Polynomial(
    repr.map(term1=>that.repr.map(term2=>product(term1,term2))).reduce(combine)
  )

  def max(that: Polynomial) = new Polynomial(
    that.repr ++ repr.transform { case (term,fac) => fac max that.coef(term) }
  )

  def min(that: Polynomial) = new Polynomial(
    repr.transform { case (term,fac) => fac min that.coef(term) }
  )

  def coef(term: Seq[String]): BigInt = repr.getOrElse(term, BigInt(0))

  override def tryCompareTo[B >: Polynomial <% PartiallyOrdered[B]](that: B): Option[Int] = that match {
    case that: Polynomial =>
      val shared = (this.repr++that.repr).keys
      var sign = 0
      shared.foreach { term =>
          val cmp = coef(term) compare that.coef(term)
          if(sign*cmp < 0) return None 
          sign |= cmp
      }
      Some(sign)
    case _ =>
      that.tryCompareTo(this).map(-_)
  }

  override def equals(that: Any) = that match {
    case that: Polynomial => tryCompareTo(that).exists(_==0)
    case x: BigInt        => this == Polynomial(x)
    case x: Int           => this == Polynomial(x)
    case _                => false
  }

  override def toString = {
    def nondigit(c: Char) = !('0' to '9' contains c)
    val str = (for((term, coef)<-repr.toSeq if coef != 0) yield coef+:term mkString "*") sortBy(-_.count(nondigit)) mkString " + "
    if(str.isEmpty) "0" else str
  }
}

object Polynomial {
  import scala.language.implicitConversions

  private def combine(a: Map[Seq[String],BigInt], b: Map[Seq[String],BigInt]) =
    a.repr ++ b.transform(a.getOrElse(_, BigInt(0))+_)

  private def product(a: (Seq[String],BigInt), b: (Seq[String],BigInt)) =
    ((a._1++b._1).sorted, a._2*b._2)

  implicit def intToPoly(value: Int): Polynomial =
    new Polynomial(Map(Seq.empty[String]->BigInt(value)))
    
  implicit def bigIntToPoly(value: BigInt): Polynomial =
    new Polynomial(Map(Seq.empty[String]->value))
    
  implicit def stringToPoly(name: String): Polynomial =
    new Polynomial(Map(Seq(name)->BigInt(1)))

  def apply(value: Int): Polynomial = value
  def apply(value: BigInt): Polynomial = value
  def apply(variable: String): Polynomial = variable

  def main(args: Array[String]) {
    val x = (Polynomial(5) * "x" + 0) * (Polynomial(2)+"x")
    println(x + 2*Polynomial("x") - Polynomial("x") - Polynomial("x")  )
    println(Polynomial(0))
  }
}

