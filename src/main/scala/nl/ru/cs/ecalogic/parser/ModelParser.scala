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

import ast._
import util.{DefaultErrorHandler, ErrorHandler}
import ModelLexer.Tokens

import scala.io.Source

import java.io.File
import scala.annotation.tailrec

class ModelParser(input: String, _errorHandler: ErrorHandler = new DefaultErrorHandler()) extends Parser(input, _errorHandler) {
  import ModelParser.First

  override protected val lexer = new ModelLexer(input)

  def numeral(follows: Pattern): BigInt = expect(Tokens.Numeral, BigInt(0))(follows)

  def model(follows: Pattern = Pattern.empty) {
    expect(Tokens.Component)(follows | Tokens.Identifier)
    val name = identifier(follows | Tokens.LParen)
    if (current(Tokens.LParen)) {
      do {
        advance()
        val variable = identifier(follows | Tokens.Colon)
        expect(Tokens.Colon)(follows | Tokens.Numeral)
        range(follows | Tokens.Comma | Tokens.RParen)
      } while(current(Tokens.Comma))
      expect(Tokens.RParen)(follows | First.modelDefinition | Tokens.End)
    }
    val definitions = sequenceOf(modelDefinition, First.modelDefinition)(follows | Tokens.End)
    expect(Tokens.End)(follows | Tokens.Component)
    expect(Tokens.Component)(follows)
  }

  def range(follows: Pattern) {
    val lower = numeral(follows | Tokens.DotDot)
    expect(Tokens.DotDot)(follows | Tokens.Numeral)
    val upper = numeral(follows)
  }

  def modelDefinition(follows: Pattern) = parse(First.modelDefinition)(follows) {
    case Tokens.Identifier(name) =>
      advance()
      expect(Tokens.Assign)(follows | Tokens.Numeral)
      numeral(follows)

      ErrorNode()
    case Tokens.Component =>
      _ => componentFunDef(follows)

    case Tokens.Function =>
      funDef(follows)

      ErrorNode()
  }

  def componentFunDef(follows: Pattern): ErrorNode = {
    val pos = position

    expect(Tokens.Component)(follows | Tokens.Function)
    expect(Tokens.Function)(follows | Tokens.Identifier)
    identifier(follows | Tokens.LParen)
    parameters(follows | Tokens.Uses | Tokens.Identifier)

    var time   = BigInt(0)
    var energy = BigInt(0)
    if (current(Tokens.Uses)) {
      advance()
      val n = numeral(follows | Tokens.Energy | Tokens.Time)

      parse(_ => unexpected(Tokens.Time | Tokens.Energy))(follows | Tokens.Identifier) {
        case Tokens.Energy =>
          advance()
          energy = n
          if (current(Tokens.Numeral)) {
            time = numeral(follows | Tokens.Time)
            expect(Tokens.Time)(follows | Tokens.Identifier)
          }
        case Tokens.Time =>
          advance()
          time = n
          if (current(Tokens.Numeral)) {
            energy = numeral(follows | Tokens.Energy)
            expect(Tokens.Energy)(follows | Tokens.Identifier)
          }
      }
    }

    composition(follows | Tokens.End)

    expect(Tokens.End)(follows | Tokens.Function)
    expect(Tokens.Function)(follows)

    ErrorNode()(pos)
  }

}

object ModelParser {

  object First {

    val modelDefinition =
      Tokens.Identifier % "<initial value definition>"      |
      Tokens.Component  % "<component function definition>" |
      Tokens.Function   % "<local function definition>"

    val transitionPart =
      Tokens.Identifier % "<variable transition>"

  }

  def main(args: Array[String]) {
    import scala.util.control.Exception._

    val file = new File(args.headOption.getOrElse(sys.error("Missing argument.")))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new ModelParser(source, errorHandler)
    val model = catching(classOf[ECAException]).opt(parser.model()).filterNot(_ => errorHandler.errorOccurred)
    println(model.getOrElse(sys.exit(1)))
  }

}
