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

import ast.ASTNode
import util.{Positional, Position, ErrorHandler}
import BaseLexer.Tokens

import scala.collection.mutable

/** Base trait for recursive descent parsers.
  *
  * @author Jascha Neutelings
  */
trait BaseParser extends Positional {
  private var recovering = false

  type ResultT <: ASTNode

  private lazy val buffer = {
    val buf = mutable.Queue.empty[(Token, Position)]
    fill(buf)
    buf
  }

  def position: Position = buffer(0)._2

  /** Returns the token at the head of the token stream. */
  protected def current: Token = buffer(0)._1

  /** Returns whether the current token matches the given pattern.
    *
    * @param p pattern
    * @return  whether the pattern matches
    */
  protected def current(p: Pattern): Boolean = p.matches(buffer(0)._1)

  /** Returns whether the ''n''-th token in the token stream matches the given pattern.
    *
    * @param p pattern
    * @param n the amount of look-ahead
    * @return  whether the pattern matches
    */
  protected def lookahead(p: Pattern, n: Int = 1): Boolean = {
    require(n <= k + 1)
    p.matches(buffer(n)._1)
  }

  /** Returns a pattern matching tokens to be ignored by the parser.
    *
    * @return pattern describing ignored tokens
    */
  protected def ignored: Pattern = Pattern.empty

  /** Returns the size of the look-ahead ''k''. */
  protected def k: Int = 1

  /** Returns the lexer used by this parser. */
  protected def lexer: BaseLexer

  /** Returns the error handler used by this parser. */
  protected def errorHandler: ErrorHandler

  private def fill(buf: mutable.Queue[(Token, Position)]) {
    while (buf.size < k + 1) lexer.next() match {
      case (Tokens.Unknown(t), p)       => errorHandler.error(new ECAException(s"Unrecognized token: '$t'.", p))
      case (t, _) if ignored.matches(t) =>
      case tp                           => buf += tp
    }
  }

  /** Advance the token stream by one token. */
  protected def advance() {
    buffer.dequeue()
    fill(buffer)
  }

  /** Advance the token stream by a number of tokens.
    *
    * @param count the number of tokens to advance by.
    */
  protected def advance(count: Int) {
    (1 to count).foreach(_ => advance())
  }

  /** Report an error on encountering an unexpected token. If the current token indicates the end of the file, a fatal
    * error is reported instead. Non-fatal errors are not reported if the parser is currently recovering.
    *
    * @param expected pattern matching the expected token.
    */
  protected def unexpected(expected: Pattern) {
    if (current(Tokens.EndOfFile))
      errorHandler.fatalError(new ECAException(s"Unexpected end of file; expected: $expected.", position))
    else if (!recovering)
      errorHandler.error(new ECAException(s"Expected: $expected; found: $current", position))
  }

  /** Tries to parse the current input using the given parser function and returns the value returned by the parser
    * function if successful (defined).
    *
    * If parsing fails, the fallback function will be called to provide a default return value and to possibly report
    * the error. At this point the parser will try to recover by throwing away tokens in the input stream until it finds
    * a token for which the parser function is defined or if the token matches a token in the follow set. In the first
    * situation, it will try to call the parser function again and in the second situation it will put the parser in
    * recovery mode and return the fallback value.
    *
    * @param  fallback fallback function
    * @param  follows  follow set pattern
    * @param  parser   parsing (partial) function
    * @tparam A        the return type of the parsing function
    * @tparam B        the return type pf the fallback function
    * @return          value returned by parsing function or fallback function
    */
  protected def tryParse[A, B <: A](fallback: Token => B)(follows: Pattern)(parser: PartialFunction[Token, A]): A = {
    if (recovering && parser.isDefinedAt(current)) // Kan beter?
      recovering = false

    parser.applyOrElse(current, { _: Token =>
      val defaultValue = fallback(current)
      while (!follows.matches(current) && !parser.isDefinedAt(current) && !current(Tokens.EndOfFile)) {
        advance()
      }
      parser.applyOrElse(current, { _: Token =>
        recovering = true
        defaultValue
      })
    })
  }

  /** Tries to match the current token against given pattern. If it succeeds, the token stream is advanced. If it fails,
    * an error is reported and recovery is performed.
    *
    * @param expected pattern to match expected token
    * @param follows  follow set pattern
    */
  protected def expect(expected: Pattern)(follows: Pattern) {
    tryParse(_ => unexpected(expected))(follows) {
      case t if expected.matches(t) => advance()
    }
  }

  protected def expect[T](expected: Pattern, default: T)(follows: Pattern): T = {
    tryParse{_ => unexpected(expected); default}(follows) {
      case t: VariableToken[T] if expected.matches(t) => advance(); t.value
    }
  }

  def expectEndOfFile() {
    expect(Tokens.EndOfFile)(Pattern.empty)
  }

  /** Advances the token stream if the current token matches the given pattern
    *
    * @param expected follow set pattern
    */
  protected def optional(expected: Pattern) {
    if (expected.matches(current)) advance()
  }

}
