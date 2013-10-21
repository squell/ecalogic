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

  def literal(follows: Pattern): Literal = parse(Tokens.Numeral, Literal(0)) (follows) {
    case Tokens.Numeral(n) => advance(); Literal(n)
  }

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
    val definitions = Seq.newBuilder
    while (!current(Tokens.End)) {
      modelDefinition(follows | Tokens.End)
    }
    advance()
    expect(Tokens.Component)(follows)
  }

  def range(follows: Pattern) {
    val lower = numeral(follows | Tokens.DotDot)
    expect(Tokens.DotDot)(follows | Tokens.Numeral)
    val upper = numeral(follows)
  }

  def modelDefinition(follows: Pattern) = parse(First.modelDefinition)(follows) {
    case Tokens.Initial =>
      advance()
      identifier(follows | Tokens.Define)
      expect(Tokens.Define)(follows | Tokens.Numeral)
      numeral(follows)

      ErrorNode()
    case Tokens.Identifier("phi") =>
      advance()
      expect(Tokens.Define)(follows | Tokens.Numeral)
      expression(follows)

      ErrorNode()
    case Tokens.Transition =>
      _ => transition(follows)
    case Tokens.Function if lookahead(Tokens.Identifier("phi")) =>
      val phi = funDef(follows)
      if (!phi.parameters.isEmpty) {
        errorHandler.error(new ECAException("Phi function should not have any parameters."))
      }

      //_ => phi
      ErrorNode()
  }

  def transition(follows: Pattern): ErrorNode = {
    expect(Tokens.Transition)(follows | Tokens.Identifier)
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

    sequenceOf(transitionPart, First.transitionPart, Tokens.End)(follows | Tokens.End)

    expect(Tokens.End)(follows | Tokens.Transition)
    expect(Tokens.Transition)(follows)

    ErrorNode()(position)
  }

  def transitionPart(follows: Pattern): ErrorNode = {
    identifier(follows | Tokens.Define)
    expect(Tokens.Define)(follows | Parser.First.expression)
    expression(follows)
    ErrorNode()(position)
  }

  def expExpr(follows: Pattern): Expression = {
    @tailrec
    def _expExpr(acc: Expression): Expression = current match {
      case Tokens.Exponent => advance(); _expExpr(Multiply(acc, literal(follows))(acc.position)) // FIXME
      case _               => acc
    }
    _expExpr(primary(follows))
  }

  override def multExpr(follows: Pattern): Expression = {
    @tailrec
    def _multExpr(acc: Expression): Expression = current match {
      case Tokens.Multiply => advance(); _multExpr(Multiply(acc, expExpr(follows))(acc.position))
      case Tokens.Divide   => advance(); _multExpr(Multiply(acc, literal(follows))(acc.position)) // FIXME
      case _               => acc
    }
    _multExpr(expExpr(follows))
  }

}

object ModelParser {

  object First {

    val modelDefinition =
      Tokens.Initial           % "<initial value definition>" |
      Tokens.Transition        % "<transition definition>"    |
      Tokens.Identifier("phi") % "<phi expression>"           |
      Tokens.Function          % "<phi function>"

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
