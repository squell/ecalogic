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

import nl.ru.cs.ecalogic.util.{DefaultErrorHandler, ErrorHandler}
import nl.ru.cs.ecalogic.SPLException
import nl.ru.cs.ecalogic.parser.Tokens._
import scala.io.Source

class Lexer(private var input: String, errorHandler: ErrorHandler = new DefaultErrorHandler()) {

  private var line = 1
  private var column = 1

  private def position = Position(line, column)

  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isIdHead(c: Char) = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

  private def isIdTail(c: Char) = isIdHead(c) || isDigit(c) || c == '_'

  private def isWhitespace(c: Char) = c == ' ' || c == '\t' || c == '\r' || c == '\n'

  private def consume(length: Int) { // Kan mogelijk efficiënter?
    input.take(length).foreach(c =>
      if (c == '\n') {
        line += 1; column = 1
      } else column += 1)
    input = input.drop(length)
  }

  def next(): (Token, Position) = {
    def lookahead(c: Char) = input.length() > 1 && input.charAt(1) == c

    val pos = position
    val (token, length) = input.headOption match {
      case None => (EndOfFile, 0)
      case Some(ch) => ch match {
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
          consume(2)
          val value = input.takeWhile(_ != '\n')

          (Comment(value.trim), value.length)

        case '(' if lookahead('*') =>
          consume(2)
          if (!input.contains("*)"))
            errorHandler.fatalError(new SPLException("Unterminated comment", pos))

          val (value, _) = input.zip(input.tail).takeWhile(_ != ('*', ')')).unzip

          (Comment(value.mkString.trim), value.length + 2)

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
            case "return"   => Return
            case "end"      => End
            case "if"       => If
            case "then"     => Then
            case "else"     => Else
            case "while"    => While
            case "upto"     => Upto
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

        case c                     => (Unknown(c), 1)
      }
    }
    consume(length)
    (token, pos)
  }
}

object Lexer {

  def main(args: Array[String]) {
    val source = Source.fromFile(args.headOption.getOrElse("zooi/test.eca")).mkString
    val lexer = new Lexer(source, new DefaultErrorHandler(source = Some(source)))
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