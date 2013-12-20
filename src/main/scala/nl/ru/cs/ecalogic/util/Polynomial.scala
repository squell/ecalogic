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
 * Future: solve max by just storing the incomparable alternatives
 *     pro: get tighter bounds
 *     con: possible data blow-up in memory
 *
 * class invariant:
 * - all Seq's used in the keys of repr must be sorted
 *
 * @author Marc Schoolderman
 */
class Polynomial private (private val repr: Map[Seq[String],BigInt], val divisor: (Seq[String],BigInt) = (Seq.empty, 1)) extends PartiallyOrdered[Polynomial] {

  import Polynomial._

  private def eqv(that: Polynomial): Map[Seq[String],BigInt] = repr.map(product(_, that.divisor))

  def +(that: Polynomial) = simplify(
    combine(this.eqv(that), that.eqv(this)),
    product(divisor, that.divisor)
  )

  def unary_- = -1*this
  def -(that: Polynomial) = this + -that

  def *(that: Polynomial) = simplify(
    repr.map(term1=>that.repr.map(term2=>product(term1,term2))).reduce(combine),
    product(divisor, that.divisor)
  )

  // would be nice to use a partialfunction here, but Scala doesn't allow that for operators?
  def /(that: Polynomial): Polynomial = {
    if(that.repr.size != 1 || that.divisor != (Seq.empty, 1))
      throw new Exception("Attempt to divide Polynomial by '$that'.")
    val term = that.repr.head
    simplify(
      repr,
      divisor match { case (vars,n) => (term._1++vars, term._2*n) }
    )
  }

  def **(const: Int) = const match {
    case n if n>=0 =>
      var acc = Polynomial(1)
      for(_ <- 1 to n) acc *= this
      acc
  }

  def max(that: Polynomial) = simplify(
    { val lifted = that.eqv(this)
      lifted ++ eqv(that).transform { case (term,fac) => fac max lifted.getOrElse(term, 0) } },
    product(divisor, that.divisor)
  )

  def min(that: Polynomial) = simplify(
    { val lifted = that.eqv(this)
      eqv(that).transform { case (term,fac) => fac min lifted.getOrElse(term, 0) } },
    product(divisor, that.divisor)
  )

  def vars: Set[String] = divisor._1.toSet ++ repr.keys.flatMap(_.toSet)

  def split: Seq[Polynomial] = repr.map(term=>simplify(Map(term),divisor)).toSeq

  def coef(term: Seq[String] = Seq.empty): BigInt = repr.getOrElse(term.sorted, BigInt(0))

  def apply[T <% Polynomial](bindings: (String,T)*): Polynomial = {
    val env: PartialFunction[String,Polynomial] = 
      bindings.toMap.mapValues(implicitly[T=>Polynomial]).orElse { case v => v }

    repr.map { case (term,fac) => fac*term.map(env).reduce(_*_) }.reduce(_+_)
  }

  /* Polynomials only admit a partial order; so beware of this. 
     This ordering compares polynomials under the assumption that all symbols
     are non-negative.  */

  override def tryCompareTo[B >: Polynomial <% PartiallyOrdered[B]](that: B): Option[Int] = that match {
    case that: Polynomial =>
      val lhs = this.eqv(that).withDefaultValue(BigInt(0))
      val rhs = that.eqv(this).withDefaultValue(BigInt(0))
      val shared = (lhs ++ rhs).keys
      var sign = 0
      shared.foreach { term =>
          val cmp = lhs(term) compare rhs(term)
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
    def prepend(coef: BigInt, term: Seq[String]) = if(coef != 1 || term.isEmpty) coef+:term else term
    val str = (for((term, coef)<-repr.toSeq if coef != 0) yield prepend(coef,term) mkString "*") sortBy(-_.count(nondigit)) mkString " + "
    val numerator = if(str.isEmpty) "0" else str
    if(divisor == (Seq.empty, 1)) 
      numerator 
    else 
      numerator + " / " + (prepend(divisor._2, divisor._1) mkString "*")
  }
}

object Polynomial {
  import scala.language.implicitConversions

  private def combine(a: Map[Seq[String],BigInt], b: Map[Seq[String],BigInt]) =
    a.repr ++ b.transform(a.getOrElse(_, BigInt(0))+_)

  private def product(a: (Seq[String],BigInt), b: (Seq[String],BigInt)) =
    ((a._1++b._1).sorted, a._2*b._2)

  private def simplify(repr: Map[Seq[String],BigInt], divisor: (Seq[String],BigInt)) = 
    if(divisor == (Seq.empty, 1) || repr.forall(_._2 == 0))
      new Polynomial(repr) 
    else {
      def gcd(x:BigInt ,y: BigInt): BigInt = if(x%y==0) y else gcd(y, x%y)
      val common = repr.foldRight(divisor) { case ((v1, n), (v2, m)) => (v1.intersect(v2), gcd(n,m)) }
      def divideOut(term: (Seq[String],BigInt)) = (term._1.diff(common._1).sorted, term._2/common._2)
      new Polynomial(repr.map(divideOut), divideOut(divisor))
    }

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
    println((Polynomial(5)*"x")**2 /5 /"x")
  }
}

