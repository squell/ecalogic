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
package ast

import model.ECAValue
import util.{Positional, Position}

/** Base sealed trait for all AST nodes.
  *
  * @author Jascha Neutelings
  */
sealed trait ASTNode extends Positional {
  private var _position = Position.default

  def position = _position

  def withPosition(p: Position): this.type = {
    _position = p
    this
  }

}

/** Dummy class representing an error during parsing. */
case class ErrorNode() extends PrimaryExpression with Statement


/** Class representing an entire ECA program. */
case class Program(imports: Map[String, Import], functions: Map[String, FunDef]) extends ASTNode

case class Import(namePath: Seq[String], alias: String) extends ASTNode {
  def qualifiedName = namePath.mkString(".")
}

/** Class representing a parameter in function definition. */
case class Param(name: String) extends ASTNode

sealed trait BasicFunction extends ASTNode {
  def name: String
  def parameters: Seq[Param]
  def body: Statement
  def arity = parameters.length
  def isComponent: Boolean
}

/** Class representing a function definition. */
case class FunDef(name: String, parameters: Seq[Param], body: Statement) extends BasicFunction {
  def isComponent = false
}



/** Base sealed trait for statement nodes. */
sealed trait Statement extends ASTNode

/** Class representing a function definition. */
case class If(predicate: Expression, consequent: Statement, alternative: Statement) extends Statement

// TODO: documentatie afmaken
case class While(predicate: Expression, rankingFunction: Option[Expression], consequent: Statement) extends Statement

case class Assignment(variable: String, value: Expression) extends Statement

case class Composition(statements: Seq[Statement]) extends Statement

case class Skip() extends Statement



sealed trait Expression extends ASTNode {
  def arity: Int
  def operands: Seq[Expression]

  def rewrite(ops: Seq[Expression]): Expression
  def transform(f: PartialFunction[Expression, Expression]): Expression =
    f.applyOrElse(rewrite(operands.map(_.transform(f))), identity[Expression])

  def foreach(f: PartialFunction[Expression, Unit]) {
    f.applyOrElse(this, (_:Expression)=>())
    operands.foreach(_.foreach(f))
  }
}


sealed trait PrimaryExpression extends Expression {
  def arity = 0
  def operands = Seq()

  def rewrite(ops: Seq[Expression]) = this
}

case class Literal(value: ECAValue) extends PrimaryExpression

case class VarRef(name: String) extends PrimaryExpression


sealed trait NAryExpression extends Expression {
  def operatorName: String
}

sealed trait BinaryExpression extends NAryExpression {
  def arity = 2
  def left: Expression
  def right: Expression
  def operands = Seq(left, right)
}

sealed trait UnaryExpression extends NAryExpression {
  def arity = 1
  def operand: Expression
  def operands = Seq(operand)
}


sealed trait LogicalExpression extends NAryExpression

case class Or(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  def operatorName = "or"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class And(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  def operatorName = "and"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}


sealed trait ArithmeticExpression extends NAryExpression

case class Add(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operatorName = "+"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Subtract(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operatorName = "-"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Multiply(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operatorName = "*"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Divide(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operatorName = "/"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Exponent(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operatorName = "^"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}


sealed trait RelationalExpression extends BinaryExpression

case class EQ(left: Expression, right: Expression) extends RelationalExpression {
  def operatorName = "="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class NE(left: Expression, right: Expression) extends RelationalExpression {
  def operatorName = "<>"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class LT(left: Expression, right: Expression) extends RelationalExpression {
  def operatorName = "<"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class LE(left: Expression, right: Expression) extends RelationalExpression {
  def operatorName = "<="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class GT(left: Expression, right: Expression) extends RelationalExpression {
  def operatorName = ">"

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class GE(left: Expression, right: Expression) extends RelationalExpression {
  def operatorName = ">="

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class FunName(name: String, prefix: Option[String] = None) {
  def qualified = prefix.map(_ + "::").getOrElse("") + name
  def isPrefixed = prefix.isDefined

  override def toString = qualified
}

case class FunCall(name: FunName, arguments: Seq[Expression]) extends NAryExpression with Statement {
  def operatorName = name.qualified
  def arity = arguments.size
  def operands = arguments

  def rewrite(ops: Seq[Expression]) = copy(arguments = ops).withPosition(position)
}





sealed trait ModelASTNode extends ASTNode

case class Initializer(name: String, value: Literal) extends ModelASTNode

case class CompVarDecl(name: String, lower: ECAValue, upper: ECAValue, initializer: Option[Initializer]) extends ModelASTNode {
  def initialValue = initializer.map(_.value.value)
}

case class Component(name: String,
                     imports: Map[String, Import],
                     variables: Map[String, CompVarDecl],
                     componentFunctions: Map[String, CompFunDef],
                     functions: Map[String, FunDef]) extends ModelASTNode

case class CompFunDef(name: String, parameters: Seq[Param], energy: ECAValue, time: ECAValue, body: Statement) extends ModelASTNode with BasicFunction {
  def isComponent = true
}
