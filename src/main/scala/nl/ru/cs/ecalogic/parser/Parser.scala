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

import scala.annotation.tailrec
import scala.collection.mutable

import nl.ru.cs.ecalogic.util.{DefaultErrorHandler, ErrorHandler}
import nl.ru.cs.ecalogic.ast._
import nl.ru.cs.ecalogic.SPLException
import scala.io.Source

final class Parser(input: String, errorHandler: ErrorHandler = new DefaultErrorHandler()) extends Lexer(input, errorHandler) {
  private var recovering = false
  private val buffer = mutable.Queue[(Token, Position)]()
  fillBuffer()

  private def currentPos: Position            = buffer(0)._2
  private def current: Token                  = buffer(0)._1
  private def current(p: Pattern): Boolean    = p.matches(buffer(0)._1)
  private def lookahead(p: Pattern): Boolean  = p.matches(buffer(1)._1)

  private def fillBuffer() {
    while (buffer.size < 2) next() match {
      case (Tokens.Comment(_), _)    =>
      case (Tokens.Whitespace(_), _) =>
      case (Tokens.Unknown(t), p)    => errorHandler.error(new SPLException(s"Unrecognized token: '$t'", p))
      case tp                        => buffer += tp
    }
  }

  private def nextToken() {
    buffer.dequeue()
    fillBuffer()
  }

  private def unexpected(expected: Any*): ErrorNode = {
    if (current(Tokens.EndOfFile))
      errorHandler.fatalError(new SPLException("Unexpected end of file", currentPos))

    val exception = expected match {
      case Seq()  => new SPLException(s"Unexpected token: $current", currentPos)
      case Seq(x) => new SPLException(s"Expected $x; found: $current", currentPos)
      case xs     => new SPLException(s"Expected any of ${xs.mkString(", ")}; found: $current", currentPos)
    }

    if (!recovering)
      errorHandler.error(exception)

    ErrorNode(Some(exception))
  }

  private def parse[A, B <: A](default: Token => B)(follows: Pattern)(f: PartialFunction[Token, A]): A = {
    if (recovering && f.isDefinedAt(current)) // Kan beter?
      recovering = false

    f.applyOrElse(current, { _: Token =>
      val defaultValue = default(current)
      while (!follows.matches(current) && !f.isDefinedAt(current) && !current(Tokens.EndOfFile)) {
        nextToken()
      }
      f.applyOrElse(current, { _: Token =>
        recovering = true
        defaultValue
      })
    })
  }

  private def parse[A >: ErrorNode <: ASTNode](expected: Any*)(follows: Pattern)(f: PartialFunction[Token, A]): A = {
    val pos = currentPos
    val res = parse[A, ErrorNode](_ => unexpected(expected:_*))(follows)(f)
    res.withPosition(pos)
  }

  private def expect(expected: Pattern)(follows: Pattern) {
    parse[Unit, Unit](_ => unexpected(expected))(follows) {
      case t if expected.matches(t) => nextToken()
    }
  }

  private def optional(expected: Pattern) {
    if (expected.matches(current)) nextToken()
  }



  def identifier(follows: Pattern): String =
    parse[String, String] {_ => unexpected("<identifier>"); "<error>"} (follows) {
      case Tokens.Identifier(n) => nextToken(); n
    }



  def program(): Program = {
    val pos = currentPos
    val definitions = Seq.newBuilder[Definition]
    while (!current(Tokens.EndOfFile)) {
      definitions += definition(Pattern.empty)
    }
    Program(definitions.result()).withPosition(pos)
  }

  def definition(follows: Pattern): FunDef = {
    val pos = currentPos

    expect(Tokens.Function)(follows)

    val name = identifier(follows | Tokens.LParen)

    val params = Seq.newBuilder[Param]
    expect(Tokens.LParen)(follows | Tokens.Identifier | Tokens.RParen)
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        val paramPos = currentPos
        val paramName = identifier(follows | Tokens.Comma | Tokens.RParen)

        params += Param(paramName).withPosition(paramPos)

        if (current(Tokens.Comma)) nextToken()
        else halt = true
      } while (!halt)
    }
    expect(Tokens.RParen)(follows | Tokens.Returns)
    expect(Tokens.Returns)(follows | Tokens.Identifier)

    val resultPos = currentPos
    val result = VarRef(identifier(follows | Tokens.Semicolon | Tokens.If | Tokens.While | Tokens.Skip | Tokens.Identifier)).withPosition(resultPos)
    optional(Tokens.Semicolon)

    val body = composition(follows | Tokens.End)
    optional(Tokens.Semicolon)

    expect(Tokens.End)(follows | Tokens.Function)
    expect(Tokens.Function)(follows)

    FunDef(name, params.result(), result, body).withPosition(pos)
  }

  def composition(follows: Pattern): Statement = {
    val pos = currentPos
    val first = statement(follows | Tokens.Semicolon)

    if (current(Tokens.Semicolon) && !lookahead(Tokens.End | Tokens.Else)) {
      val statements = Seq.newBuilder += first

      do {
        nextToken()
        statements += statement(follows | Tokens.Semicolon)
      } while (current(Tokens.Semicolon) && !lookahead(Tokens.End | Tokens.Else))

      Composition(statements.result()).withPosition(pos)
    } else
      first
  }

  def statement(follows: Pattern) =
    parse[Statement]("<skip statement>", "<if statement>", "<while statement>", "<assignment>", "<function call>")(follows) {
      case Tokens.If =>
        nextToken()
        val predicate = expression(follows | Tokens.Then)

        expect(Tokens.Then)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
        val consequent = composition(follows | Tokens.Then | Tokens.Semicolon)
        optional(Tokens.Semicolon)

        expect(Tokens.Else)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
        val alternative = composition(follows | Tokens.End | Tokens.Semicolon)
        optional(Tokens.Semicolon)

        expect(Tokens.End)(follows | Tokens.If)
        expect(Tokens.If)(follows)

        If(predicate, consequent, alternative)
      case Tokens.While =>
        nextToken()
        val predicate = expression(follows | Tokens.Upto)

        expect(Tokens.Upto)(follows | Tokens.Identifier | Tokens.Numeral | Tokens.LParen)
        val rankingFunction = expression(follows | Tokens.Do)

        expect(Tokens.Do)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
        val consequent = composition(follows | Tokens.End | Tokens.Semicolon)
        optional(Tokens.Semicolon)

        expect(Tokens.End)(follows | Tokens.While)
        expect(Tokens.While)(follows)

        While(predicate, rankingFunction, consequent)
      case Tokens.Identifier(n) if lookahead(Tokens.LParen | Tokens.ColonColon) =>
        funCall(follows)
      case Tokens.Identifier(n) if lookahead(Tokens.Assign) =>
        nextToken()
        nextToken()
        val expr = expression(follows)

        Assignment(VarRef(n), expr)
      case Tokens.Skip =>
        nextToken()

        Skip()
    }


  def funCall(follows: Pattern): FunCall = {
    val compOrNamePart = identifier(follows | Tokens.ColonColon | Tokens.LParen)

    val name = if (current(Tokens.ColonColon)) {
      nextToken()
      val namePart = identifier(follows | Tokens.LParen)

      FunName(namePart, Some(compOrNamePart))
    } else
      FunName(compOrNamePart)

    expect(Tokens.LParen)(follows | Tokens.Identifier | Tokens.Numeral | Tokens.LParen | Tokens.RParen)
    val arguments = Seq.newBuilder[Expression]
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        arguments += expression(follows | Tokens.Comma | Tokens.RParen)

        if (current(Tokens.Comma)) nextToken()
        else halt = true
      } while(!halt)
    }
    expect(Tokens.RParen)(follows)

    FunCall(name, arguments.result())
  }

  def expression(follows: Pattern): Expression = orExpr(follows)()

  def primary(follows: Pattern): Expression =
    parse[Expression]("<natural number>", "<function call>", "<variable reference>", "<parenthesized expression>")(follows) {
      case Tokens.Identifier(n) if lookahead(Tokens.LParen | Tokens.ColonColon) =>
        funCall(follows)
      case Tokens.Identifier(n) =>
        nextToken()

        VarRef(n)
      case Tokens.Numeral(v) =>
        nextToken()

        Literal(v)
      case Tokens.LParen =>
        nextToken()

        val expr = expression(follows | Tokens.RParen)
        expect(Tokens.RParen)(follows)

        expr
    }

  @tailrec
  def multExpr(follows: Pattern)(acc: Expression = primary(follows)): Expression = current match {
    case Tokens.Multiply => nextToken(); multExpr(follows)(Multiply(acc, primary(follows)).withPosition(acc))
    case _               => acc
  }

  @tailrec
  def addExpr(follows: Pattern)(acc: Expression = multExpr(follows)()): Expression = current match {
    case Tokens.Plus  => nextToken(); addExpr(follows)(Add     (acc, multExpr(follows)()).withPosition(acc))
    case Tokens.Minus => nextToken(); addExpr(follows)(Subtract(acc, multExpr(follows)()).withPosition(acc))
    case _            => acc
  }

  def relExpr(follows: Pattern): Expression = {
    val left = addExpr(follows)()
    current match {
      case Tokens.LT => nextToken(); LT(left, addExpr(follows)()).withPosition(left)
      case Tokens.LE => nextToken(); LE(left, addExpr(follows)()).withPosition(left)
      case Tokens.GT => nextToken(); GT(left, addExpr(follows)()).withPosition(left)
      case Tokens.GE => nextToken(); GE(left, addExpr(follows)()).withPosition(left)
      case Tokens.EQ => nextToken(); EQ(left, addExpr(follows)()).withPosition(left)
      case Tokens.NE => nextToken(); NE(left, addExpr(follows)()).withPosition(left)
      case _         => left
    }
  }

  @tailrec
  def andExpr(follows: Pattern)(acc: Expression = relExpr(follows)): Expression = current match {
    case Tokens.And => nextToken(); andExpr(follows)(And(acc, relExpr(follows)).withPosition(acc))
    case _          => acc
  }

  @tailrec
  def orExpr(follows: Pattern)(acc: Expression = andExpr(follows)()): Expression = current match {
    case Tokens.Or => nextToken(); orExpr(follows)(Or(acc, andExpr(follows)()).withPosition(acc))
    case _         => acc
  }

}

object Parser {

  def main(args: Array[String]) {
    val source = Source.fromFile(args.headOption.getOrElse("zooi/test.eca")).mkString
    val parser = new Parser(source, new DefaultErrorHandler(source = Some(source)))
    println(parser.program())
  }

}