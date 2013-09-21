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

  private def currentPos: Position = buffer(0)._2
  private def current: Token       = buffer(0)._1
  private def current(t: Token*)   = t.exists(buffer(0)._1.matches)
  private def lookahead(t: Token*) = t.exists(buffer(1)._1.matches)

  private def fillBuffer() {
    while (buffer.size < 2) next() match {
      case (Tokens.Comment(_), _)    =>
      case (Tokens.Whitespace(_), _) =>
      case (Tokens.Unknown(t), p)    => errorHandler.error(new SPLException("Unrecognized token: '"+t+"'", p))
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
      case Seq()  => new SPLException("Unexpected token: " + current, currentPos)
      case Seq(x) => new SPLException("Expected " + x + "; found: " + current, currentPos)
      case xs     => new SPLException("Expected any of " + xs.mkString(", ") + "; found: " + current, currentPos)
    }
    if (!recovering)
      errorHandler.error(exception)
    nextToken()
    ErrorNode(Some(exception))
  }

  private def parse[A >: B, B](default: Token => B)(follows: Token => Boolean)(f: PartialFunction[Token, A]): A = {
    if (f.isDefinedAt(current)) {
      recovering = false
      f(current)
    } else {
      val defaultValue = default(current)
      while (!follows(current) && !f.isDefinedAt(current) && !current(Tokens.EndOfFile)) {
        nextToken()
      }
      if (f.isDefinedAt(current))
        f(current)
      else {
        recovering = true
        defaultValue
      }
    }
  }

  private def parse[A >: ErrorNode <: ASTNode](expected: Any*)(follows: Token => Boolean)(f: PartialFunction[Token, A]): A = {
    val pos = currentPos
    val res = parse[A, ErrorNode](_ => unexpected(expected:_*))(follows)(f)
    res.withPosition(pos)
  }

  private def expect(expected: Token*)(follows: Set[Token]) {
    parse[Unit, Unit](_ => unexpected(expected:_*))(follows) {
      case t if expected.exists(t.matches) => nextToken()
    }
  }

  private def optional(expected: Token*) {
    if (expected.contains(current)) nextToken()
  }



  def identifier(follows: Set[Token]): String =
    parse[String, String] {_ => unexpected("<identifier>"); "<error>"} (follows) {
      case Tokens.KeywordOrIdentifier(n) => nextToken(); n
    }



  def program(): Program = {
    val pos = currentPos
    val definitions = mutable.Queue[Definition]()
    while (!current(Tokens.EndOfFile)) {
      definitions += definition(Set())
    }
    Program(definitions).withPosition(pos)
  }

  def definition(follows: Set[Token]): FunDef = {
    val pos = currentPos
    
    expect(Tokens.Function)(follows)
    
    val name = identifier(follows + Tokens.LParen)
                      
    val params = mutable.Queue[Param]()
    expect(Tokens.LParen)(follows + Tokens.KeywordOrIdentifier + Tokens.RParen)
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        val paramPos = currentPos
        val paramName = identifier(follows + Tokens.Comma + Tokens.RParen)

        params += Param(paramName).withPosition(paramPos)

        if (current(Tokens.Comma)) nextToken()
        else halt = true
      } while (!halt)
    }
    expect(Tokens.RParen)(follows + Tokens.KeywordOrIdentifier + Tokens.Skip + Tokens.If + Tokens.While)

    val body = composition(follows + Tokens.Semicolon)

    expect(Tokens.Semicolon)(follows + Tokens.Return)

    expect(Tokens.Return)(follows + Tokens.KeywordOrIdentifier)
    val resultPos = currentPos
    val result = VarRef(Left(identifier(follows + Tokens.Semicolon + Tokens.End))).withPosition(resultPos)
    optional(Tokens.Semicolon)
    
    expect(Tokens.End)(follows + Tokens.Function)
    expect(Tokens.Function)(follows)

    FunDef(name, params, body, result).withPosition(pos)
  }

  def composition(follows: Set[Token]): Statement = {
    val pos = currentPos
    val first = statement(follows + Tokens.Semicolon)

    if (current(Tokens.Semicolon) && !lookahead(Tokens.Return, Tokens.End, Tokens.Else)) {
      var statements = mutable.Queue(first)

      do {
        nextToken()
        statements += statement(follows + Tokens.Semicolon)
      } while (current(Tokens.Semicolon) && !lookahead(Tokens.Return, Tokens.End, Tokens.Else))

      Composition(statements).withPosition(pos)
    } else
      first
  }

  def statement(follows: Set[Token]) = parse[Statement]("<skip statement>", "<if statement>", "<while statement>", "<assignment>", "<function call>")(follows) {
    case Tokens.If =>
      nextToken()
      val predicate = expression(follows + Tokens.Then)

      expect(Tokens.Then)(follows + Tokens.KeywordOrIdentifier + Tokens.Skip + Tokens.If + Tokens.While)
      val consequent = composition(follows + Tokens.Then + Tokens.Semicolon)
      optional(Tokens.Semicolon)

      expect(Tokens.Else)(follows + Tokens.KeywordOrIdentifier + Tokens.Skip + Tokens.If + Tokens.While)
      val alternative = composition(follows + Tokens.End + Tokens.Semicolon)
      optional(Tokens.Semicolon)

      expect(Tokens.End)(follows + Tokens.If)
      expect(Tokens.If)(follows)

      If(predicate, consequent, alternative)
    case Tokens.While =>
      nextToken()
      val predicate = expression(follows + Tokens.Upto)

      expect(Tokens.Upto)(follows + Tokens.KeywordOrIdentifier + Tokens.Numeral + Tokens.LParen)
      val rankingFunction = expression(follows + Tokens.Do)

      expect(Tokens.Do)(follows + Tokens.KeywordOrIdentifier + Tokens.Skip + Tokens.If + Tokens.While)
      val consequent = composition(follows + Tokens.End + Tokens.Semicolon)
      optional(Tokens.Semicolon)

      expect(Tokens.End)(follows + Tokens.While)
      expect(Tokens.While)(follows)

      While(predicate, rankingFunction, consequent)
    case Tokens.KeywordOrIdentifier(n) if lookahead(Tokens.LParen, Tokens.ColonColon) =>
      funCall(follows)
    case Tokens.KeywordOrIdentifier(n) if lookahead(Tokens.Assign) =>
      nextToken()
      nextToken()
      val expr = expression(follows)

      Assignment(VarRef(Left(n)), expr)
    case Tokens.Skip =>
      nextToken()

      Skip()

  }


  def funCall(follows: Set[Token]): FunCall = {
    val compOrNamePart = identifier(follows + Tokens.ColonColon + Tokens.LParen)

    val name = if (current(Tokens.ColonColon)) {
      nextToken()
      val namePart = identifier(follows + Tokens.LParen)

      FunName(namePart, Some(compOrNamePart))
    } else
      FunName(compOrNamePart)

    expect(Tokens.LParen)(follows + Tokens.KeywordOrIdentifier + Tokens.Numeral + Tokens.LParen + Tokens.RParen)
    val arguments = mutable.Queue[Expression]()
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        arguments += expression(follows + Tokens.Comma + Tokens.RParen)

        if (current(Tokens.Comma)) nextToken()
        else halt = true
      } while(!halt)
    }
    expect(Tokens.RParen)(follows)

    FunCall(Left(name), arguments)
  }

  def expression(follows: Set[Token]): Expression = orExpr(follows)()

  def primary(follows: Set[Token]): Expression = parse[Expression]("<natural number>", "<function call>", "<variable reference>", "<parenthesized expression>")(follows) {
    case Tokens.KeywordOrIdentifier(n) if lookahead(Tokens.LParen, Tokens.ColonColon) =>
      funCall(follows)
    case Tokens.KeywordOrIdentifier(n) =>
      nextToken()

      VarRef(Left(n))
    case Tokens.Numeral(v) =>
      nextToken()

      Literal(v)
    case Tokens.LParen =>
      nextToken()

      val expr = expression(follows + Tokens.RParen)
      expect(Tokens.RParen)(follows)

      expr
  }

  @tailrec
  def multExpr(follows: Set[Token])(acc: Expression = primary(follows)): Expression = current match {
    case Tokens.Multiply => nextToken(); multExpr(follows)(Multiply(acc, primary(follows)).withPosition(acc))
    case _               => acc
  }

  @tailrec
  def addExpr(follows: Set[Token])(acc: Expression = multExpr(follows)()): Expression = current match {
    case Tokens.Plus  => nextToken(); addExpr(follows)(Add     (acc, multExpr(follows)()).withPosition(acc))
    case Tokens.Minus => nextToken(); addExpr(follows)(Subtract(acc, multExpr(follows)()).withPosition(acc))
    case _            => acc
  }

  def relExpr(follows: Set[Token]): Expression = {
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
  def andExpr(follows: Set[Token])(acc: Expression = relExpr(follows)): Expression = current match {
    case Tokens.And => nextToken(); andExpr(follows)(And(acc, relExpr(follows)).withPosition(acc))
    case _          => acc
  }

  @tailrec
  def orExpr(follows: Set[Token])(acc: Expression = andExpr(follows)()): Expression = current match {
    case Tokens.Or => nextToken(); orExpr(follows)(Or(acc, andExpr(follows)()).withPosition(acc))
    case _         => acc
  }

}

object Parser {

  def main(args: Array[String]) {
    val source = Source.fromFile(args.headOption.getOrElse("zooi/stress.eca")).mkString
    val parser = new Parser(source, new DefaultErrorHandler(source = Some(source)))
    println(parser.program())
  }

}