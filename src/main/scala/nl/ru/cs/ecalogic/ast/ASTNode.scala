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

package nl.ru.cs.ecalogic.ast

import nl.ru.cs.ecalogic.util.{Positional, Position}

/** Base trait for all AST nodes.
  *
  * @author Jascha Neutelings
  */
sealed trait ASTNode extends Positional

/** Dummy class representing an error during parsing. */
case class ErrorNode()(val position: Position) extends PrimaryExpression with Statement


/** Class representing an entire ECA program. */
case class Program(definitions: Seq[FunDef])(val position: Position) extends ASTNode

/** Class representing a parameter in function definition. */
case class Param(name: String)(val position: Position) extends ASTNode

/** Class representing a function definition. */
case class FunDef(name: String, parameters: Seq[Param], result: VarRef, body: Statement)(val position: Position) extends ASTNode



/** Base trait for statement nodes. */
trait Statement extends ASTNode

/** Class representing a function definition. */
case class If(predicate: Expression, consequent: Statement, alternative: Statement)(val position: Position) extends Statement

// TODO: documentatie afmaken
case class While(predicate: Expression, rankingFunction: Expression, consequent: Statement)(val position: Position) extends Statement

case class Assignment(variable: String, value: Expression)(val position: Position) extends Statement

case class Composition(statements: Seq[Statement])(val position: Position) extends Statement

case class Skip()(val position: Position) extends Statement



trait Expression extends ASTNode {
  def arity: Int
  def operands: Seq[Expression]

  def rewrite(ops: Seq[Expression]): Expression
  def transform(f: PartialFunction[Expression, Expression]): Expression =
    f.applyOrElse(rewrite(operands.map(_.transform(f))), identity : Expression => Expression)
}


trait PrimaryExpression extends Expression {
  def arity = 0
  def operands = Seq()

  def rewrite(ops: Seq[Expression]) = this
}

case class Literal(value: BigInt)(val position: Position) extends PrimaryExpression

case class VarRef(name: String)(val position: Position) extends PrimaryExpression


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

case class Or(left: Expression, right: Expression)(val position: Position) extends BinaryExpression with LogicalExpression {
  def operator = "||"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class And(left: Expression, right: Expression)(val position: Position) extends BinaryExpression with LogicalExpression {
  def operator = "&&"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}


trait ArithmeticExpression extends NAryExpression

case class Add(left: Expression, right: Expression)(val position: Position) extends BinaryExpression with ArithmeticExpression {
  def operator = "+"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class Subtract(left: Expression, right: Expression)(val position: Position) extends BinaryExpression with ArithmeticExpression {
  def operator = "-"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class Multiply(left: Expression, right: Expression)(val position: Position) extends BinaryExpression with ArithmeticExpression {
  def operator = "*"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}


trait RelationalExpression extends BinaryExpression

case class EQ(left: Expression, right: Expression)(val position: Position) extends RelationalExpression {
  def operator = "="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class NE(left: Expression, right: Expression)(val position: Position) extends RelationalExpression {
  def operator = "<>"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class LT(left: Expression, right: Expression)(val position: Position) extends RelationalExpression {
  def operator = "<"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class LE(left: Expression, right: Expression)(val position: Position) extends RelationalExpression {
  def operator = "<="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class GT(left: Expression, right: Expression)(val position: Position) extends RelationalExpression {
  def operator = ">"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class GE(left: Expression, right: Expression)(val position: Position) extends RelationalExpression {
  def operator = ">="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1))(position)
}

case class FunName(name: String, component: Option[String] = None) {
  def qualified = component.map(_ + "::").getOrElse("") + name
  def isComponentFunction = component.isDefined

  override def toString = qualified
}

case class FunCall(name: FunName, arguments: Seq[Expression])(val position: Position) extends NAryExpression with Statement {
  def operator = name.qualified
  def arity = arguments.size
  def operands = arguments

  def rewrite(ops: Seq[Expression]) = copy(arguments = ops)(position)
}
