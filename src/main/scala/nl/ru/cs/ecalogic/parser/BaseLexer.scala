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

import BaseLexer.Tokens._
import nl.ru.cs.ecalogic.util.{Positional, Position}

/** Base trait for lexers.
  *
  * @author Jascha Neutelings
  */
trait BaseLexer extends Positional {
  private var line = 1
  private var column = 1

  /** The input string. */
  protected var input: String

  def position = Position(line, column)

  private def consume(length: Int) {
    val (consumed, newInput) = input.splitAt(length)
    consumed.foreach {
      case '\n' => line += 1; column = 1
      case _    => column += 1
    }
    input = newInput
  }

  /** Checks whether the ''n''-th character in the input stream matches the given character.
    *
    * @param c character to look for.
    * @param n the amount of look-ahead
    * @return  whether the character matches
    */
  protected def lookahead(c: Char, n: Int = 1) = input.length > n && input.charAt(n) == c

  //TODO: doc
  def next(): (Token, Position) = {
    def unknown(c: Char) = (Unknown(c), 1)

    val pos = position
    val (token, length) = input.headOption.map(parseToken.applyOrElse(_, unknown)).getOrElse((EndOfFile, 0))
    consume(length)
    (token, pos)
  }

  /** A partial function that takes the current character and returns the recognized token (if any) and length of the
    * matched string.
    */
  protected val parseToken: PartialFunction[Char, (Token, Int)]

}

object BaseLexer {

  /** Basic tokens. */
  object Tokens {
    case object EndOfFile extends Token {
      override def toString = "<end-of-file>"
    }

    case class Unknown(value: Char) extends VariableToken[Char]("unknown")
  }

}
