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

import scala.io.Source
import java.io.File
import Lexer.Tokens._

/** Lexer for ECA programs.
 *
 * @param input input string to parse
 *
 * @author Jascha Neutelings
 */
class Lexer(protected var input: String) extends BaseLexer {
  import Lexer._

  protected val parseToken: PartialFunction[Char, (Token, Int)] = {
    case '+'                   => (Plus, 1)
    case '-'                   => (Minus, 1)
    case '*'                   => (Multiply, 1)

    case '='                   => (EQ, 1)
    case '<' if lookahead('=') => (LE, 2)
    case '<' if lookahead('>') => (NE, 2)
    case '<'                   => (LT, 1)
    case '>' if lookahead('=') => (GE, 2)
    case '>'                   => (GT, 1)
    case ':' if lookahead(':') => (ColonColon, 2)
    case ':' if lookahead('=') => (Assign, 2)

    case '/' if lookahead('/') =>
      val end = input.indexOf('\n', 2)
      val value = if (end >= 0) input.substring(2, end) else input.substring(2)
      (Comment(value.trim), value.length + 2)

    case '(' if lookahead('*') =>
      val end = input.indexOf("*)", 2)
      //if (end < 0) errorHandler.fatalError(new SPLException("Unterminated comment", position))
      val value = if (end >= 0) input.substring(2, end) else input.substring(2)
      (Comment(value.trim), value.length + 4)

    case '('                   => (LParen, 1)
    case ')'                   => (RParen, 1)
    case ','                   => (Comma, 1)
    case ';'                   => (Semicolon, 1)

    case d if isDigit(d)       =>
      val value = input.takeWhile(isDigit)
      (Numeral(BigInt(value)), value.length)

    case h if isIdHead(h)      =>
      val value = input.takeWhile(isIdTail)
      val token = value match {
        case "function" => Function
        case "end"      => End
        case "if"       => If
        case "then"     => Then
        case "else"     => Else
        case "while"    => While
        case "bound"    => Bound
        case "do"       => Do
        case "skip"     => Skip
        case "and"      => And
        case "or"       => Or

        case v          => Identifier(v)
      }
      (token, value.length)

    case w if isWhitespace(w)  =>
      val value = input.takeWhile(isWhitespace)
      (Whitespace(value), value.length)
  }

}

object Lexer {

  def isDigit(c: Char) = c >= '0' && c <= '9'

  def isIdHead(c: Char) = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'

  def isIdTail(c: Char) = isIdHead(c) || isDigit(c)

  def isWhitespace(c: Char) = c == ' ' || c == '\t' || c == '\r' || c == '\n'


  /** Tokens for the ECA program lexer. */
  object Tokens {
    // Make base tokens available
    val EndOfFile = BaseLexer.Tokens.EndOfFile
    val Unknown   = BaseLexer.Tokens.Unknown

    case object Function                 extends Keyword("function")
    case object End                      extends Keyword("end")

    case object If                       extends Keyword("if")
    case object Then                     extends Keyword("then")
    case object Else                     extends Keyword("else")

    case object While                    extends Keyword("while")
    case object Bound                    extends Keyword("bound")
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



    // Patterns
    object Identifier extends TypePattern[Identifier] {
      override def toString = "<identifier>"
    }

    object Numeral extends TypePattern[Numeral] {
      override def toString = "<natural number>"
    }

    object Comment extends TypePattern[Comment] {
      override def toString = "<comment>"
    }

    object Whitespace extends TypePattern[Whitespace] {
      override def toString = "<whitespace>"
    }

  }



  def main(args: Array[String]) {
    val file = new File(args.headOption.getOrElse(sys.error("Missing argument.")))
    val source = Source.fromFile(file).mkString
    val lexer = new Lexer(source)

    var (token, _) = lexer.next()
    while (token != EndOfFile) {
      token match {
        case Whitespace(_) | Comment(_) =>
        case t                          => println(t)
      }
      token = lexer.next()._1
    }

  }

}
