package nl.ru.cs.ecalogic.parser

import scala.annotation.tailrec
import scala.collection.mutable

import nl.ru.cs.ecalogic.util.{DefaultErrorHandler, ErrorHandler}
import nl.ru.cs.ecalogic.ast._
import nl.ru.cs.ecalogic.SPLException
import scala.io.Source

final class Parser(input: String, errorHandler: ErrorHandler = new DefaultErrorHandler()) extends Lexer(input, errorHandler) {
  private val buffer = mutable.Queue[(Token, Position)]()
  fillBuffer()

  private def currentPos: Position         = buffer(0)._2
  private def current: Token               = buffer(0)._1
  private def current(t: TokenTemplate)    = buffer(0)._1.matches(t)
  private def lookahead(t: TokenTemplate)  = buffer(1)._1.matches(t)

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

  private def unexpected(expected: TokenTemplate*) {
    if (current(Tokens.EndOfFile))
      errorHandler.fatalError(new SPLException("Unexpected end of file", currentPos))

    val exception = expected match {
      case Seq()  => new SPLException("Unexpected token: " + current, currentPos)
      case Seq(x) => new SPLException("Expected " + x + "; found: " + current, currentPos)
      case xs     => new SPLException("Expected any of " + xs.mkString(", ") + "; found: " + current, currentPos)
    }
    errorHandler.error(exception)
    nextToken()
  }

  private def expect(template: TokenTemplate) {
    if (current.matches(template)) nextToken()
    else unexpected(template)
  }

  def identifier(): String = current match {
    case Tokens.Identifier(n) => nextToken(); n
    case t => unexpected(Wildcards.Identifier); "<error>"
  }



  def program(): Program = {
    val pos = currentPos
    val definitions = mutable.Queue[Definition]()
    while (!current(Tokens.EndOfFile)) {
      definitions += definition()
    }
    Program(definitions).withPosition(pos)
  }

  def definition(): FunDef = {    
    val pos = currentPos
    
    expect(Tokens.Function)
    
    val name = identifier()
                      
    val params = mutable.Queue[Param]()
    expect(Tokens.LParen)
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        val paramPos = currentPos
        val paramName = identifier()

        params += Param(paramName).withPosition(paramPos)

        if (current(Tokens.Comma)) nextToken()
        else halt = true
      } while (!halt)
    }
    expect(Tokens.RParen)

    val body = composition()

    expect(Tokens.Semicolon)
    expect(Tokens.Return)
    val resultPos = currentPos
    val result = VarRef(Left(identifier())).withPosition(resultPos)
    
    expect(Tokens.End)
    expect(Tokens.Function)

    FunDef(name, params, body, result).withPosition(pos)
  }

  def composition(): Statement = {
    val pos = currentPos
    val first = statement()

    if (current(Tokens.Semicolon) && !lookahead(Tokens.Return)) {
      var statements = mutable.Queue(first)

      do {
        nextToken()
        statements += statement()
      } while (current(Tokens.Semicolon) && !lookahead(Tokens.Return))

      Composition(statements).withPosition(pos)
    } else
      first
  }

  def statement(): Statement = {
    val pos = currentPos
    val res = current match {
      case Tokens.Skip =>
        nextToken()
        
        Skip()      
      case Tokens.If =>
        nextToken()
        val predicate = expression()
        
        expect(Tokens.Then)
        val consequent = composition()
        
        expect(Tokens.Else)        
        val alternative = composition()

        expect(Tokens.End)
        expect(Tokens.If)

        If(predicate, consequent, alternative)
      case Tokens.While =>
        nextToken()
        val predicate = expression()

        expect(Tokens.Upto)
        val rankingFunction = expression()

        expect(Tokens.Do)
        val consequent = composition()

        expect(Tokens.End)
        expect(Tokens.While)

        While(predicate, rankingFunction, consequent)
      case Tokens.Identifier(n) =>
        if (lookahead(Tokens.LParen) || lookahead(Tokens.ColonColon)) {
          funCall()
        } else {
          nextToken()
          expect(Tokens.Assign)
          val expr = expression()

          Assignment(VarRef(Left(n)), expr)
        }
      case _ =>
        unexpected(Tokens.Skip, Tokens.If, Tokens.While, Wildcards.Identifier)
        if (current(Tokens.Semicolon)) nextToken()
        ErrorNode()
    }
    res.withPosition(pos)
  }

  def funCall(): FunCall = {
    val compOrNamePart = identifier()

    val name = if (current(Tokens.ColonColon)) {
      nextToken()
      val namePart = identifier()

      FunName(namePart, Some(compOrNamePart))
    } else
      FunName(compOrNamePart)

    expect(Tokens.LParen)
    val arguments = mutable.Queue[Expression]()
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        arguments += expression()

        if (current(Tokens.Comma)) nextToken()
        else halt = true
      } while(!halt)
    }
    expect(Tokens.RParen)

    FunCall(Left(name), arguments)
  }

  def expression(): Expression = orExpr()

  def primary(): Expression = {
    val pos = currentPos
    val res = current match {
      case Tokens.Identifier(n) if lookahead(Tokens.LParen) || lookahead(Tokens.ColonColon) => funCall()
      case Tokens.Identifier(n) => nextToken(); VarRef(Left(n))
      case Tokens.IntLiteral(v) => nextToken(); Literal(v)
      case Tokens.LParen =>
        nextToken()

        val expr = expression()
        expect(Tokens.RParen)

        expr
      case _ =>
        unexpected(Wildcards.Identifier, Wildcards.IntLiteral, Tokens.LParen)
        ErrorNode()
    }
    res.withPosition(pos)
  }

  @tailrec
  def multExpr(acc: Expression = primary()): Expression = current match {
    case Tokens.Multiply => nextToken(); multExpr(Multiply(acc, primary()).withPosition(acc))
    case _               => acc
  }

  @tailrec
  def addExpr(acc: Expression = multExpr()): Expression = current match {
    case Tokens.Plus  => nextToken(); addExpr(Add     (acc, multExpr()).withPosition(acc))
    case Tokens.Minus => nextToken(); addExpr(Subtract(acc, multExpr()).withPosition(acc))
    case _            => acc
  }

  def relExpr(): Expression = {
    val left = addExpr()
    current match {
      case Tokens.LT => nextToken(); LT(left, addExpr()).withPosition(left)
      case Tokens.LE => nextToken(); LE(left, addExpr()).withPosition(left)
      case Tokens.GT => nextToken(); GT(left, addExpr()).withPosition(left)
      case Tokens.GE => nextToken(); GE(left, addExpr()).withPosition(left)
      case Tokens.EQ => nextToken(); EQ(left, addExpr()).withPosition(left)
      case Tokens.NE => nextToken(); NE(left, addExpr()).withPosition(left)
      case _         => left
    }
  }

  @tailrec
  def andExpr(acc: Expression = relExpr()): Expression = current match {
    case Tokens.And => nextToken(); andExpr(And(acc, relExpr()).withPosition(acc))
    case _          => acc
  }

  @tailrec
  def orExpr(acc: Expression = andExpr()): Expression = current match {
    case Tokens.Or => nextToken(); orExpr(Or(acc, andExpr()).withPosition(acc))
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