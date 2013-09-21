package nl.ru.cs.ecalogic.ast

import nl.ru.cs.ecalogic.SPLException
import nl.ru.cs.ecalogic.parser.{Position, Positional}

sealed abstract class ASTNode extends Positional {
  private var pos: Option[Position] = None

  def position = pos

  def withPosition(p: Position): this.type = {
    require(p != null)
    pos = Some(p)
    this
  }

  def withPosition(p: Positional): this.type = p.position match {
    case Some(position) => withPosition(position)
    case _ => this
  }
}

case class ErrorNode(message: Option[SPLException] = None) extends PrimaryExpression
                                                           with Definition
                                                           with Statement



case class Program(definitions: Seq[Definition]) extends ASTNode

trait Definition extends ASTNode

case class Param(name: String) extends ASTNode

case class FunDef(name: String, parameters: Seq[Param], body: Statement, result: VarRef) extends Definition



trait Statement extends ASTNode

case class If(predicate: Expression, consequent: Statement, alternative: Statement) extends Statement

case class While(predicate: Expression, rankingFunction: Expression, consequent: Statement) extends Statement

case class Assignment(variable: VarRef, expression: Expression) extends Statement

case class Composition(statements: Seq[Statement]) extends Statement

case class Skip() extends Statement



trait Expression extends ASTNode {
  def arity: Int
  def operands: Seq[Expression]

  def rewrite(ops: Seq[Expression]): Expression
  def transform(f: PartialFunction[Expression, Expression]): Expression = f.applyOrElse(rewrite(operands.map(_.transform(f))), identity : Expression => Expression)
}


trait PrimaryExpression extends Expression {
  def arity = 0
  def operands = Seq()

  def rewrite(ops: Seq[Expression]) = this
}

case class Literal(value: BigInt) extends PrimaryExpression

case class VarRef(variable: Either[String, VarDecl]) extends PrimaryExpression {
  def name = variable.fold(identity, _.name)
}


trait NAryExpression extends Expression {
  def operator: String
}

trait BinaryExpression extends NAryExpression {
  def arity = 2
  def left: Expression
  def right: Expression
  def operands = Seq(left, right)
}

trait UnaryExpression extends NAryExpression {
  def arity = 1
  def operand: Expression
  def operands = Seq(operand)
}


trait LogicalExpression extends NAryExpression

case class Or(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  def operator = "||"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class And(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  def operator = "&&"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}


trait ArithmeticExpression extends NAryExpression

case class Add(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "+"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class Subtract(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "-"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class Multiply(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "*"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}


trait RelationalExpression extends BinaryExpression

case class EQ(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class NE(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "<>"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class LT(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "<"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class LE(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "<="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class GT(left: Expression, right: Expression) extends RelationalExpression {
  def operator = ">"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class GE(left: Expression, right: Expression) extends RelationalExpression {
  def operator = ">="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(this)
}

case class FunName(name: String, component: Option[String] = None) {
  def qualified = component.map(_ + "::").getOrElse("") + name
  def isComponentFunction = component.isDefined

  override def toString = qualified
}

case class FunCall(function: Either[FunName, FunDecl], arguments: Seq[Expression]) extends NAryExpression with Statement {
  def name = function.fold(_.qualified, _.name)
  def operator = name
  def arity = arguments.size
  def operands = arguments

  def rewrite(ops: Seq[Expression]) = copy(arguments = ops).withPosition(this)
}

// TODO: Vervang dummy classes
case class FunDecl() {
  def name: String = ???
}

case class VarDecl() {
  def name: String = ???
}
