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
package parser

import scala.reflect.ClassTag

/** Trait for matching against tokens.
  *
  * @author Jascha Neutelings
  */
trait Pattern {

  private case class WithDescription(description: String) extends Pattern {
    def matches(token: Token) = Pattern.this.matches(token)

    override def toString = description
  }

  /** Returns whether the given token matches this pattern
    *
    * @param token token to match against
    * @return      whether it matches
    */
  def matches(token: Token): Boolean

  /** Combines this pattern with the given pattern in the form of a union.
    *
    * @param other pattern to combine with
    * @return      union of this pattern and ''other''
    */
  def |(other: Pattern): Pattern = Pattern.union(this, other)

  /** Add a descriptive string to this pattern and return the result.
    *
    * @param description new description
    * @return            pattern with description
    */
  def %(description: String): Pattern = WithDescription(description)

}

/** Object with utility functions for manipulating patterns
  *
  * @author Jascha Neutelings
  */
object Pattern {

  private case class MultiPattern(patterns: Set[Pattern]) extends Pattern {
    require(patterns.size >= 2)

    def matches(token: Token) = patterns.exists(_.matches(token))
    override def %(description: String): Pattern = sys.error("Not allowed")

    override def toString =
      if (patterns.size == 2) s"${patterns.head} or ${patterns.last}"
      else s"${patterns.init.mkString(", ")} or ${patterns.last}"
  }

  private case object NilPattern extends Pattern {
    def matches(token: Token) = false
    override def %(description: String): Pattern = sys.error("Not allowed")

    override def toString = "<nothing>"
  }

  /** Returns a pattern that does not match any token. */
  def empty: Pattern = NilPattern

  /** Combines a set of patterns to form a new pattern.
    *
    * @param patterns set of patterns
    * @return         union of given patterns
    */
  def union(patterns: Set[Pattern]): Pattern =  {
    val patSet = patterns.flatMap {
      case NilPattern       => Set[Pattern]()
      case MultiPattern(ps) => ps
      case p                => Set(p)
    }
    if      (patSet.isEmpty)   NilPattern
    else if (patSet.size == 1) patSet.head
    else                       MultiPattern(patSet)
  }

  /** Combines a set of patterns to form a new pattern.
    *
    * @param patterns patterns
    * @return         union of given patterns
    */
  def union(patterns: Pattern*): Pattern = union(patterns.toSet)

}

/** Abstract class for pattern for matching tokens purely based on their type.
  *
  * @tparam T the type to match against
  *
  * @author Jascha Neutelings
  */
abstract class TypePattern[T <: Token : ClassTag] extends Pattern {
  def matches(token: Token) = implicitly[ClassTag[T]].runtimeClass.isInstance(token)
}
