package nl.ru.cs.ecalogic.parser

import scala.annotation.tailrec
import scala.collection.mutable

import nl.ru.cs.ecalogic.util.{CachingErrorHandler, DefaultErrorHandler, ErrorHandler}
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
      case (tokens.Comment(_), _)    =>
      case (tokens.Whitespace(_), _) =>
      case (tokens.Unknown(t), p)    => errorHandler.error(new SPLException("Unrecognized token: '"+t+"'", p))
      case tp                        => buffer += tp
    }
  }
  
  private def nextToken() {
    buffer.dequeue()
    fillBuffer()
  }

  private def unexpected(expected: TokenTemplate*) {
    if (current(tokens.EndOfFile))
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
    case tokens.Identifier(n) => nextToken(); n
    case t => unexpected(wildcards.Identifier); "<error>"
  }



  def program(): Program = {
    val pos = currentPos
    val definitions = mutable.Buffer[Definition]()
    while (!current(tokens.EndOfFile)) {
      definitions += definition()
    }
    Program(definitions).withPosition(pos)
  }

  def definition(): FunDef = {    
    val pos = currentPos
    
    expect(tokens.Function)
    
    val name = identifier()
                      
    val params = mutable.Buffer[Param]()
    expect(tokens.LParen)
    if (!current(tokens.RParen)) {
      var halt = false
      do {
        val paramPos = currentPos
        val paramName = identifier()

        params += Param(paramName).withPosition(paramPos)

        if (current(tokens.Comma)) nextToken()
        else halt = true
      } while (!halt)
    }
    expect(tokens.RParen)

    val body = statementList()

    expect(tokens.Return)
    val resultPos = currentPos
    val result = VarRef(Left(identifier())).withPosition(resultPos)
    
    expect(tokens.End)
    expect(tokens.Function)

    FunDef(name, params, body, result).withPosition(pos)
  }

  def statementList(): Statement = {
    val pos = currentPos
    val first = statement()

    if (current(tokens.Semicolon)) {
      var statements = mutable.Buffer(first)

      do {
        nextToken()
        if (!current(tokens.Return))
          statements += statement()
      } while (current(tokens.Semicolon))

      StatementList(statements).withPosition(pos)
    } else
      first
  }

  def statement(): Statement = {
    val pos = currentPos
    val res = current match {
      case tokens.Skip =>
        nextToken()
        
        Skip()      
      case tokens.If =>
        nextToken()
        val predicate = expression()
        
        expect(tokens.Then)
        val consequent = statementList()
        
        expect(tokens.Else)        
        val alternative = statementList()

        expect(tokens.End)
        expect(tokens.If)

        If(predicate, consequent, alternative)
      case tokens.While =>
        nextToken()
        val predicate = expression()

        expect(tokens.Upto)
        val rankingFunction = expression()

        expect(tokens.Do)
        val consequent = statementList()

        expect(tokens.End)
        expect(tokens.While)

        While(predicate, rankingFunction, consequent)
      case tokens.Identifier(n) =>
        if (lookahead(tokens.LParen) || lookahead(tokens.ColonColon)) {
          funCall()
        } else {
          nextToken()
          expect(tokens.Assign)
          val expr = expression()

          Assignment(VarRef(Left(n)), expr)
        }
      case _ =>
        unexpected(tokens.Skip, tokens.If, tokens.While, wildcards.Identifier)
        if (current(tokens.Semicolon)) nextToken()
        ErrorNode()
    }
    res.withPosition(pos)
  }

  def funCall(): FunCall = {
    val compOrNamePart = identifier()

    val name = if (current(tokens.ColonColon)) {
      nextToken()
      val namePart = identifier()

      FunName(namePart, Some(compOrNamePart))
    } else
      FunName(compOrNamePart)

    expect(tokens.LParen)
    val arguments = mutable.Buffer[Expression]()
    if (!current(tokens.RParen)) {
      var halt = false
      do {
        arguments += expression()

        if (current(tokens.Comma)) nextToken()
        else halt = true
      } while(!halt)
    }
    expect(tokens.RParen)

    FunCall(Left(name), arguments)
  }

  def expression(): Expression = orExpr()

  def primary(): Expression = {
    val pos = currentPos
    val res = current match {
      case tokens.Identifier(n) if lookahead(tokens.LParen) || lookahead(tokens.ColonColon) => funCall()
      case tokens.Identifier(n) => nextToken(); VarRef(Left(n))
      case tokens.IntLiteral(v) => nextToken(); Literal(v)
      case tokens.LParen =>
        nextToken()

        val expr = expression()
        expect(tokens.RParen)

        expr
      case _ =>
        unexpected(wildcards.Identifier, wildcards.IntLiteral, tokens.LParen)
        ErrorNode()
    }
    res.withPosition(pos)
  }

  @tailrec
  def multExpr(acc: Expression = primary()): Expression = current match {
    case tokens.Multiply => nextToken(); multExpr(Multiply(acc, primary()).withPosition(acc))
    case _               => acc
  }

  @tailrec
  def addExpr(acc: Expression = multExpr()): Expression = current match {
    case tokens.Plus  => nextToken(); addExpr(Add     (acc, multExpr()).withPosition(acc))
    case tokens.Minus => nextToken(); addExpr(Subtract(acc, multExpr()).withPosition(acc))
    case _            => acc
  }

  def relExpr(): Expression = {
    val left = addExpr()
    current match {
      case tokens.LT => nextToken(); LT(left, addExpr()).withPosition(left)
      case tokens.LE => nextToken(); LE(left, addExpr()).withPosition(left)
      case tokens.GT => nextToken(); GT(left, addExpr()).withPosition(left)
      case tokens.GE => nextToken(); GE(left, addExpr()).withPosition(left)
      case tokens.EQ => nextToken(); EQ(left, addExpr()).withPosition(left)
      case tokens.NE => nextToken(); NE(left, addExpr()).withPosition(left)
      case _         => left
    }
  }

  @tailrec
  def andExpr(acc: Expression = relExpr()): Expression = current match {
    case tokens.And => nextToken(); andExpr(And(acc, relExpr()).withPosition(acc))
    case _          => acc
  }

  @tailrec
  def orExpr(acc: Expression = andExpr()): Expression = current match {
    case tokens.Or => nextToken(); orExpr(Or(acc, andExpr()).withPosition(acc))
    case _         => acc
  }

}

object Parser {

  def main(args: Array[String]) {
    val source = Source.fromFile(args.headOption.getOrElse("test.eca")).mkString
    val parser = new Parser(source)
    try {
      println(parser.program())
    } finally {
      Console.err.flush()
    }
  }

}