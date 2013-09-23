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

package nl.ru.cs.ecalogic.parser

trait Pattern {
  def matches(token: Token): Boolean

  def |(pattern: Pattern): Pattern = Pattern.union(this, pattern)
}

object Pattern {
  private case class MultiPattern(patterns: Set[Pattern]) extends Pattern {
    def matches(token: Token) = patterns.exists(_.matches(token))
  }

  private case object NilPattern extends Pattern {
    def matches(token: Token) = false
  }

  def empty: Pattern = NilPattern

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

  def union(patterns: Pattern*): Pattern = union(patterns.toSet)
}



sealed trait Token extends Pattern {
  def matches(token: Token) = this == token
}

sealed abstract class FixedToken(fixedValue: String) extends Token {
  override def toString = "'" + fixedValue + "'"
}

sealed abstract class Keyword(val keyword: String) extends FixedToken(keyword)

sealed abstract class VariableToken[T](name: String) extends Token {
  def value: T

  override def toString = "<" + name + " : '" + value + "'>"
}

object Tokens {

  case object Function                 extends Keyword("function")
  case object Return                   extends Keyword("return")
  case object End                      extends Keyword("end")

  case object If                       extends Keyword("if")
  case object Then                     extends Keyword("then")
  case object Else                     extends Keyword("else")

  case object While                    extends Keyword("while")
  case object Upto                     extends Keyword("upto")
  case object Do                       extends Keyword("do")

  case object Skip                     extends Keyword("skip")

  case class Numeral(value: BigInt)    extends VariableToken[BigInt]("numeral")
  case class Identifier(value: String) extends VariableToken[String]("identifier")

  case object Assign                   extends FixedToken(":=")

  case object Plus                     extends FixedToken("+")
  case object Minus                    extends FixedToken("-")
  case object Multiply                 extends FixedToken("*")

  case object EQ                       extends FixedToken("=")
  case object LT                       extends FixedToken("<")
  case object GT                       extends FixedToken(">")
  case object LE                       extends FixedToken("<=")
  case object GE                       extends FixedToken(">=")
  case object NE                       extends FixedToken("<>")

  case object And                      extends FixedToken("and")
  case object Or                       extends FixedToken("or")

  case object LParen                   extends FixedToken("(")
  case object RParen                   extends FixedToken(")")

  case object Comma                    extends FixedToken(",")
  case object Semicolon                extends FixedToken(";")

  case object ColonColon               extends FixedToken("::")

  case class Comment(value: String)    extends VariableToken[String]("comment")
  case class Whitespace(value: String) extends VariableToken[String]("whitespace")
  case object EndOfFile                extends Token {
    override def toString = "<end-of-file>"
  }

  case class Unknown(value: Char)      extends VariableToken[Char]("unknown")

  object Identifier                    extends Pattern {
    def matches(that: Token) = that match {
      case Identifier(_) => true
      case _             => false
    }

    override def toString = "<identifier>"
  }

  object Numeral                       extends Pattern {
    def matches(that: Token) = that match {
      case Numeral(_) => true
      case _          => false
    }

    override def toString = "<natural number>"
  }

// Disabled because it messes with error recovery

//  object KeywordOrIdentifier           extends Pattern {
//    def matches(that: Token) = unapply(that).isDefined
//
//    def unapply(t: Token): Option[String] = t match {
//      case Tokens.Identifier(n) => Some(n)
//      case k: Keyword           => Some(k.keyword)
//      case _                    => None
//    }
//  }

}
