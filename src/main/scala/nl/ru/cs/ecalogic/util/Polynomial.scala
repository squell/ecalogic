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
 * @author Marc Schoolderman
 */
class Polynomial private (private val repr: Map[Seq[String],Int]) {

  import Polynomial._

  def +(that: Polynomial) = new Polynomial(combine(this.repr, that.repr))

  def *(that: Polynomial) = new Polynomial(
    repr.toSeq.map(term1=>that.repr.map(term2=>product(term1,term2))).reduce(combine)
  )

  override def toString = {
    def nondigit(c: Char) = !('0' to '9' contains c)
    repr.toSeq.map { term=>(term._2 +: term._1) mkString "*" } sortBy {-_.count(nondigit)} mkString " + "
  }
}

object Polynomial {
  import scala.language.implicitConversions

  private def combine(a: Map[Seq[String],Int], b: Map[Seq[String],Int]) =
    a.repr ++ b.transform(a.repr.getOrElse(_,0)+_)

  private def product(a: (Seq[String],Int), b: (Seq[String],Int)) =
    ((a._1++b._1).sorted, a._2*b._2)

  implicit def constToPoly(value: Int): Polynomial =
    new Polynomial(Map(Seq.empty[String]->value).withDefaultValue(0))
    
  implicit def stringToPoly(name: String): Polynomial =
    new Polynomial(Map(Seq(name)->1).withDefaultValue(0))

  def apply(value: Int): Polynomial = value
  def apply(variable: String): Polynomial = variable

  def main(args: Array[String]) {
    val x = (Polynomial(5) * "x" + 4) * (Polynomial(2)+"x")
    println(x)
  }
}

