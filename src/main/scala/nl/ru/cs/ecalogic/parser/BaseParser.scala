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

import nl.ru.cs.ecalogic.util.ErrorHandler
import scala.collection.mutable
import nl.ru.cs.ecalogic.parser.BaseLexer.Tokens
import nl.ru.cs.ecalogic.SPLException

abstract class BaseParser(lexer: BaseLexer, errorHandler: ErrorHandler) extends Positional {
  private var recovering = false

  private lazy val buffer = {
    val buf = mutable.Queue[(Token, Position)]()
    fill(buf)
    buf
  }

  def position: Position                       = buffer(0)._2
  protected def current: Token                 = buffer(0)._1
  protected def current(p: Pattern): Boolean   = p.matches(buffer(0)._1)
  protected def lookahead(p: Pattern): Boolean = p.matches(buffer(1)._1)

  protected def ignored: Pattern = Pattern.empty

  private def fill(buf: mutable.Queue[(Token, Position)]) {
    while (buf.size < 2) lexer.next() match {
      case (Tokens.Unknown(t), p)       => errorHandler.error(new SPLException(s"Unrecognized token: '$t'", p))
      case (t, _) if ignored.matches(t) =>
      case tp                           => buf += tp
    }
  }

  protected def advance(count: Int = 1) {
    (1 to count).foreach { _ =>
      buffer.dequeue()
      fill(buffer)
    }
  }

  protected def unexpected(expected: Any*) {
    if (current(Tokens.EndOfFile))
      errorHandler.fatalError(new SPLException("Unexpected end of file", position))

    val exception = expected match {
      case Seq()  => new SPLException(s"Unexpected token: $current", position)
      case Seq(x) => new SPLException(s"Expected $x; found: $current", position)
      case xs     => new SPLException(s"Expected any of ${xs.mkString(", ")}; found: $current", position)
    }

    if (!recovering)
      errorHandler.error(exception)
  }

  protected def parse[A, B <: A](default: Token => B)(follows: Pattern)(f: PartialFunction[Token, A]): A = {
    if (recovering && f.isDefinedAt(current)) // Kan beter?
      recovering = false

    f.applyOrElse(current, { _: Token =>
      val defaultValue = default(current)
      while (!follows.matches(current) && !f.isDefinedAt(current) && !current(Tokens.EndOfFile)) {
        advance()
      }
      f.applyOrElse(current, { _: Token =>
        recovering = true
        defaultValue
      })
    })
  }

  protected def expect(expected: Pattern)(follows: Pattern) {
    parse[Unit, Unit](_ => unexpected(expected))(follows) {
      case t if expected.matches(t) => advance()
    }
  }

  protected def optional(expected: Pattern) {
    if (expected.matches(current)) advance()
  }

}
