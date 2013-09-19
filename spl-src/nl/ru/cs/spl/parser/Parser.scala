package nl.ru.cs.spl.parser

import scala.annotation.tailrec
import scala.collection.mutable

import nl.ru.cs.spl.util.{DefaultErrorHandler, ErrorHandler}
import nl.ru.cs.spl.ast._
import nl.ru.cs.spl.SPLException

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
    //exception
  }

  private def expect(template: TokenTemplate) {
    if (current.matches(template)) nextToken()
    else unexpected(template)
  }

  private def skipTo(templates: TokenTemplate*) {
//    while (!current(tokens.EndOfFile) && !templates.exists(current.matches(_))) {
//      nextToken()
//    }
  }

  def reportError(message: String, pos: Option[Position]): ErrorNode = {
    val e = new SPLException(message, pos)
    errorHandler.error(e)
    ErrorNode(Some(e))
  }
  
  def illegalVoid(pos: Position) = reportError("Void is not a valid type in this context.", Some(pos))

  def identifier(follows: TokenTemplate*): String = current match {
    case tokens.Identifier(n) => nextToken(); n
    case t => unexpected(wildcards.Identifier); skipTo(follows:_*); "<error>"
  }



  def program(): Program = {
    val pos = currentPos
    val definitions = mutable.Buffer[Definition]()
    while (!current(tokens.EndOfFile)) {
      definitions += definition()
    }
    Program(definitions).withPosition(pos)
  }

  def definition(): Definition = {
    val pos = currentPos
    var t: Type = current match {
      case tokens.Void => nextToken(); VoidType().withPosition(pos)
      case _           => tpe(allowVoid = true)
    }

    if (lookahead(tokens.LParen)) funDefPart(t).withPosition(pos)
    else {
      if (t == VoidType()) t = illegalVoid(pos)
      varDefPart(t, global = true).withPosition(pos)
    }
  }

  def varDefPart(t: Type, global: Boolean): VarDef = {
    val name = identifier(tokens.Assign)

    expect(tokens.Assign)
    val expr = expression()

    expect(tokens.Semicolon)

    VarDef(name, t, expr, global = global)
  }

  def funDefPart(t: Type): FunDef = {
    val name = identifier(tokens.LParen)
                      
    val params = mutable.Buffer[Param]()
    expect(tokens.LParen)
    if (!current(tokens.RParen)) {
      var halt = false
      do {
        val paramType = tpe()
        val paramName = identifier(tokens.RParen, tokens.Comma)

        params += Param(paramName, paramType).withPosition(paramType)

        if (current(tokens.Comma)) nextToken()
        else halt = true
      } while (!halt)
    }
    expect(tokens.RParen)

    val pos = currentPos
    val body = compound().withPosition(pos)

    FunDef(name, t, params, body)
  }

  def tpe(allowVoid: Boolean = false): Type = {
    val pos = currentPos
    val res = current match {
      case tokens.Int    => nextToken(); IntType()
      case tokens.Bool   => nextToken(); BoolType()
      case tokens.LParen =>
        nextToken()
        val first = tpe()
        expect(tokens.Comma)
        val second = tpe()
        expect(tokens.RParen)

        TupleType(first, second)
      case tokens.LSquare =>
        nextToken()
        val elemType = tpe()
        expect(tokens.RSquare)

        ListType(elemType)
      case tokens.Identifier(n) => nextToken(); TypeParam(n)
      case tokens.Void =>
        if (allowVoid) {
          nextToken()
          VoidType()
        } else {
          val pos = currentPos
          nextToken()
          illegalVoid(pos)
        }
      case _ =>
        unexpected(tokens.Int, tokens.Bool, tokens.LCurly, tokens.LSquare, wildcards.Identifier)
        skipTo(wildcards.Identifier)
        ErrorNode()
    }
    res.withPosition(pos)
  }

  def compound(): Compound = {
    def startOfVarDef = current match {
      case tokens.Int | tokens.Bool | tokens.LParen | tokens.LSquare => true
      case tokens.Identifier(_) => lookahead(wildcards.Identifier)
      case _ => false
    }
    //val pos = currentPos
    expect(tokens.LCurly)
    var atBody = false
    var vars = mutable.Buffer[VarDef]()
    var stmts = mutable.Buffer[Statement]()
    while (!current(tokens.RCurly)) {
      if (startOfVarDef) {
        val pos = currentPos
        val variableType = tpe()
        val varDecl = varDefPart(variableType, global = false).withPosition(pos)
        if (atBody) {
          val e = new SPLException("Variable definitions have to be at the start of a block statement.", pos)
          errorHandler.error(e)
          stmts += ErrorNode(Some(e)).withPosition(varDecl)
        } else {
          vars += varDecl
        }
      } else {
        stmts += statement()
        atBody = true
      }
    }
    nextToken()

    Compound(vars, stmts)//.withPosition(pos
  }

  def statement(): Statement = {
    val pos = currentPos
    val res = current match {
      case tokens.LCurly =>
        compound()
      case tokens.If =>
        nextToken()

        expect(tokens.LParen)
        val predicate = expression()
        expect(tokens.RParen)

        val consequent = statement()
        val alternative = if (current(tokens.Else)) {nextToken(); Some(statement())} else None

        If(predicate, consequent, alternative)
      case tokens.While =>
        nextToken()

        expect(tokens.LParen)
        val predicate = expression()
        expect(tokens.RParen)

        val consequent = statement()

        While(predicate, consequent)
      case tokens.Identifier(n) =>
        if (lookahead(tokens.LParen)) {
          val fc = funCall().withPosition(pos)
          expect(tokens.Semicolon)
          ProcCall(fc)
        } else {
          nextToken()
          expect(tokens.Assign)
          val expr = expression()
          expect(tokens.Semicolon)

          Assignment(VarRef(Left(n)), expr)
        }
      case tokens.Return =>
        nextToken()
        val expr = if (!current(tokens.Semicolon)) Some(expression()) else None
        expect(tokens.Semicolon)
        Return(expr)
      case _ =>
        unexpected(tokens.LCurly, tokens.If, tokens.While, tokens.Return, wildcards.Identifier)
        skipTo(tokens.Semicolon, tokens.RCurly)
        if (current(tokens.Semicolon)) nextToken()
        ErrorNode()
    }
    res.withPosition(pos)
  }

  def expression(): Expression = consExpr()

  def term(): Expression = {
    val pos = currentPos
    val res = current match {
      case tokens.Identifier(n) if lookahead(tokens.LParen) => funCall()
      case tokens.Identifier(n) => nextToken(); VarRef(Left(n))
      case tokens.IntLiteral(v) => nextToken(); Literal(IntValue(v))
      case tokens.False         => nextToken(); Literal(False)
      case tokens.True          => nextToken(); Literal(True)
      case tokens.LSquare =>
        nextToken()
        expect(tokens.RSquare)

        Literal(NilValue)
      case tokens.LParen =>
        nextToken()

        val left = expression()
        val expr = current match {
          case tokens.Comma => nextToken(); TupleCons(left, expression())
          case _            => left
        }
        expect(tokens.RParen)

        expr
      case _ =>
        unexpected(tokens.False, tokens.True, tokens.RSquare, tokens.LParen, wildcards.Identifier)
        skipTo(tokens.RParen, tokens.Semicolon, tokens.Comma)
        ErrorNode()
    }
    res.withPosition(pos)
  }

  def funCall(): FunCall = {
    val name = identifier()

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

  def unaryExpr(): Expression = {
    val pos = currentPos
    current match {
      case tokens.Not   => nextToken(); Not(unaryExpr())   .withPosition(pos)
      case tokens.Minus => nextToken(); Negate(unaryExpr()).withPosition(pos)
      case _            => term()
    }
  }

  @tailrec
  def multExpr(acc: Expression = unaryExpr()): Expression = current match {
    case tokens.Multiply => nextToken(); multExpr(Multiply(acc, unaryExpr()).withPosition(acc))
    case tokens.Divide   => nextToken(); multExpr(Divide  (acc, unaryExpr()).withPosition(acc))
    case tokens.Modulo   => nextToken(); multExpr(Modulo  (acc, unaryExpr()).withPosition(acc))
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

  def consExpr(): Expression = {
    val left = orExpr()
    current match {
      case tokens.Cons => nextToken(); Cons(left, consExpr()).withPosition(left)
      case _           => left
    }
  }

}