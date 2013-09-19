package nl.ru.cs.ecalogic.ast

import nl.ru.cs.ecalogic.SPLException
import nl.ru.cs.ecalogic.parser.{Position, Positional}

sealed abstract class ASTNode extends Positional {
  private var pos: Option[Position] = None

  def children: Seq[ASTNode]
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

  def foreach[U](f: ASTNode => U) {
    children.foreach(f)
  }
}

trait LeafNode extends ASTNode {
  final def children = Seq()

  override def foreach[U](f: ASTNode => U) {}
}

case class ErrorNode(message: Option[SPLException] = None) extends PrimaryExpression
                                                           with Definition
                                                           with Statement
                                                           with Expression {
  def resultType = sys.error("Not implemented")
  def declaration = sys.error("Not implemented")
  def optimize(operands: Seq[Expression]) = sys.error("Not implemented")
}

case class Program(definitions: Seq[Definition]) extends ASTNode {
  def children = definitions
}

trait Definition extends ASTNode

case class Param(name: String) extends LeafNode

case class FunDef(name: String, parameters: Seq[Param], body: Statement, result: VarRef) extends Definition  {
  def children = parameters ++ Seq(body, result)
}


trait Statement extends ASTNode

case class If(predicate: Expression, consequent: Statement, alternative: Statement) extends Statement {
  def children = Seq(predicate, consequent, alternative)
}

case class While(predicate: Expression, rankingFunction: Expression, consequent: Statement) extends Statement {
  def children = Seq(predicate, rankingFunction, consequent)
}

case class Assignment(variable: VarRef, expression: Expression) extends Statement {
  def children = Seq(expression)
}

case class StatementList(statements: Seq[Statement]) extends Statement {
  def children = statements
}

case class Skip() extends Statement with LeafNode


trait Expression extends ASTNode {
  def arity: Int
  def operands: Seq[Expression]

  def rewrite(ops: Seq[Expression]): Expression
  def transform(f: PartialFunction[Expression, Expression]): Expression = f.applyOrElse(rewrite(operands.map(_.transform(f))), identity : Expression => Expression)
}


trait PrimaryExpression extends Expression with LeafNode {
  def arity = 0
  def operands = Seq()
  def operandTypes = Seq()
  def rewrite(ops: Seq[Expression]) = this
}

case class Literal(value: BigInt) extends PrimaryExpression

case class VarRef(variable: Either[String, VarDecl]) extends PrimaryExpression {
  def name = variable.fold(identity, _.name)
}

trait NAryExpression extends Expression {
  def operator: String
  def children = operands
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
