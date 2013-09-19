package nl.ru.cs.spl.ast

import nl.ru.cs.spl.SPLException
import nl.ru.cs.spl.parser.{Position, Positional}

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

  def withInfo(node: ASTNode): this.type = withPosition(node)

  def foreach[U](f: ASTNode => U) {
    children.foreach(f)
  }
}

trait LeafNode extends ASTNode {
  final def children = Seq()

  override def foreach[U](f: ASTNode => U) {}
}

case class ErrorNode(message: Option[SPLException] = None) extends Term
                                                           with PrimitiveType
                                                           with Definition
                                                           with Statement
                                                           with Expression {
  type DeclT = Nothing
  def resultType = sys.error("Not implemented")
  def declaration = sys.error("Not implemented")
  def optimize(operands: Seq[Expression]) = sys.error("Not implemented")
}



trait Type extends ASTNode {
  def subTypes: Seq[Type]
  def rewrite(types: Seq[Type]): Type
  def transform(f: PartialFunction[Type, Type]): Type = f.applyOrElse(rewrite(subTypes.map(_.transform(f))), identity: Type => Type)
  def isComposite: Boolean
  def isVoid = false
}

trait PrimitiveType extends Type with LeafNode {
  def subTypes = Seq()
  def rewrite(ops: Seq[Type]) = this
  def isComposite = false
}

case class BoolType() extends PrimitiveType {
  override def toString = "Bool"
}

case class IntType() extends PrimitiveType {
  override def toString = "Int"
}

case class VoidType() extends PrimitiveType {
  override def toString = "Void"
  override def isVoid = true
}

trait CompositeType extends Type {
  def children = subTypes
  def isComposite = true
}

case class ListType(elementType: Type) extends CompositeType {
  def subTypes = Seq(elementType)

  def rewrite(types: Seq[Type]) = copy(elementType = types(0)).withInfo(this)

  override def toString = "[" + elementType + "]"
}

case class TupleType(firstType: Type, secondType: Type) extends CompositeType {
  def subTypes = Seq(firstType, secondType)

  def rewrite(types: Seq[Type]) = copy(firstType = types(0), secondType = types(1)).withInfo(this)

  override def toString = "(" + firstType + ", " + secondType + ")"
}

case class TypeParam(name: String) extends PrimitiveType {
  override def toString = name
}

case class TypeArg(name: Either[String, Int]) extends PrimitiveType {
  override def toString = name.fold(identity, "$" + _)
}

case object WildcardType extends PrimitiveType {
  override def toString = "*"
}




trait Value {
  type ScalaT
  def scalaValue: ScalaT
  def tpe: Type
}

sealed trait BoolValue extends Value {
  type ScalaT = Boolean
  def tpe = BoolType()
}

object BoolValue {
  def apply(b: Boolean) = b match {
    case true => True
    case _    => False
  }

  def unapply(b: BoolValue) = Some(b.scalaValue)
}

case object True extends BoolValue {
  def scalaValue = true
}

case object False extends BoolValue {
  def scalaValue = false
}


case class IntValue(scalaValue: BigInt) extends Value {
  type ScalaT = BigInt
  def tpe = IntType()

  override def toString = scalaValue.toString()
}


trait ListValue[T <: Value] extends Value {
  type ScalaT = List[T]
  def tpe = ListType(TypeArg(Left("t")))
}

case class ConsValue[T <: Value](head: T, tail: ListValue[T]) extends ListValue[T] {
  def scalaValue = head :: tail.scalaValue

  override def toString = head + " : " + tail
}

case object NilValue extends ListValue[Nothing] {
  def scalaValue = Nil

  override def toString = "[]"
}


case class TupleValue[A <: Value, B <: Value](a: A, b: B) extends Value {
  type ScalaT = (A,B)
  def tpe = TupleType(TypeArg(Left("a")), TypeArg(Left("b")))

  def scalaValue = (a, b)

  override def toString = "(" + a + ", " + b + ")"
}


case class Program(definitions: Seq[Definition]) extends ASTNode {
  def children = definitions
}

trait Definition extends ASTNode {
  type DeclT <: Symbol
  def declaration: DeclT
}

case class Param(name: String, tpe: Type) extends Definition {
  type DeclT = VarDecl
  def children = Seq(tpe)
  def declaration = VarDecl(name, tpe, VarType.Param)
}

case class FunDef(name: String, resultType: Type, parameters: Seq[Param], body: Compound) extends Definition  {
  type DeclT = FunDecl
  def children = Seq(resultType) ++ parameters :+ body

  private val replaceTypeParams: PartialFunction[Type, Type] = {
    case TypeParam(n) => TypeArg(Left(n))
  }

  def declaration = FunDecl(name, resultType.transform(replaceTypeParams), parameters.map(_.tpe.transform(replaceTypeParams)))
}

case class VarDef(name: String, tpe: Type, value: Expression, global: Boolean) extends Definition {
  type DeclT = VarDecl
  def children = Seq(tpe, value)
  def declaration = VarDecl(name, tpe, if (global) VarType.Global else VarType.Local)
}


trait Statement extends ASTNode

case class If(predicate: Expression, consequent: Statement, alternative: Option[Statement]) extends Statement {
  def children = Seq(predicate, consequent) ++ alternative
}

case class While(predicate: Expression, consequent: Statement) extends Statement {
  def children = Seq(predicate, consequent)
}

case class Assignment(variable: VarRef, expression: Expression) extends Statement {
  def children = Seq(expression)
}

case class Compound(variables: Seq[VarDef], statements: Seq[Statement]) extends Statement {
  def children = variables ++ statements
}

//object Compound {
//  def empty = Compound(Seq(), Seq())
//}

case class Return(expression: Option[Expression]) extends Statement {
  def children = expression.toSeq
}

case class ProcCall(fun: FunCall) extends Statement {
  def children = Seq(fun)
}


trait Expression extends ASTNode {
  private var tpe: Option[Type] = None

  def typeInfo = tpe

  def withTypeInfo(t: Type): this.type = {
    require(t != null)
    //assert(tpe.isEmpty)
    tpe = Some(t)
    this
  }

  override def withInfo(node: ASTNode): this.type = node match {
    case e: Expression if e.typeInfo.isDefined => super.withInfo(e).withTypeInfo(e.typeInfo.get)
    case n => super.withInfo(n)
  }

  def arity: Int
  def operands: Seq[Expression]
  def operandTypes: Seq[Type]
  def resultType: Type
  def optimize(operands: Seq[Expression]): Expression
  def checkValidity: Option[String] = None

  def rewrite(ops: Seq[Expression]): Expression
  def transform(f: PartialFunction[Expression, Expression]): Expression = f.applyOrElse(rewrite(operands.map(_.transform(f))), identity : Expression => Expression)
}


trait Term extends Expression with LeafNode {
  def arity = 0
  def operands = Seq()
  def operandTypes = Seq()
  def rewrite(ops: Seq[Expression]) = this
}

case class Literal[T <: Value](value: T) extends Term {
  def resultType: Type = value.tpe
  def optimize(operands: Seq[Expression]) = this
}

case class VarRef(variable: Either[String, VarDecl]) extends Term {
  def name = variable.fold(identity, _.name)
  def resultType: Type = variable.right.get.tpe
  def optimize(operands: Seq[Expression]) = this
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


trait LogicalExpression extends NAryExpression {
  def operandTypes = Seq.fill(arity)(BoolType())
  def resultType = BoolType()
}

case class Or(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  def operator = "||"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(BoolValue(x)), Literal(BoolValue(y))) => Literal(BoolValue(x || y))
    case s                                                 => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class And(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  def operator = "&&"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(BoolValue(x)), Literal(BoolValue(y))) => Literal(BoolValue(x && y))
    case s                                                 => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class Not(operand: Expression) extends UnaryExpression with LogicalExpression {
  def operator = "!"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(True))   => Literal(False)
    case Seq(Literal(False))  => Literal(True)
    case s                    => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(operand = ops(0)).withInfo(this)
}


trait ArithmeticExpression extends NAryExpression {
  def operandTypes = Seq.fill(arity)(IntType())
  def resultType = IntType()
}

case class Add(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "+"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(IntValue(x + y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class Subtract(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "-"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(IntValue(x - y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class Multiply(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "*"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(IntValue(x * y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class Divide(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "/"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(IntValue(x / y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)

  override def checkValidity = right match {
    case Literal(IntValue(x)) if x == 0 => Some("Division by zero")
    case _                              => None
  }
}

case class Modulo(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  def operator = "%"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(IntValue(x % y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)

  override def checkValidity = right match {
    case Literal(IntValue(x)) if x == 0 => Some("Division by zero")
    case _                              => None
  }
}

case class Negate(operand: Expression) extends UnaryExpression with ArithmeticExpression {
  def operator = "-"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x))) => Literal(IntValue(-x))
    case s                         => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(operand = ops(0)).withInfo(this)
}


trait RelationalExpression extends BinaryExpression {
  def operandTypes = Seq.fill(arity)(IntType())
  def resultType = BoolType()
}

case class EQ(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "=="

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(BoolValue(x == y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class NE(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "!="

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(BoolValue(x != y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class LT(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "<"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(BoolValue(x < y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class LE(left: Expression, right: Expression) extends RelationalExpression {
  def operator = "<="

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(BoolValue(x <= y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class GT(left: Expression, right: Expression) extends RelationalExpression {
  def operator = ">"

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(BoolValue(x > y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class GE(left: Expression, right: Expression) extends RelationalExpression {
  def operator = ">="

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(IntValue(x)), Literal(IntValue(y))) => Literal(BoolValue(x >= y))
    case s                                               => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}


case class Cons(left: Expression, right: Expression) extends BinaryExpression {
  def operator = ":"

  def operandTypes = Seq(TypeArg(Left("t")), ListType(TypeArg(Left("t"))))
  def resultType = ListType(TypeArg(Left("t")))

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(x), Literal(xs: ListValue[Value])) => Literal(ConsValue(x, xs))
    case s                                              => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class TupleCons(left: Expression, right: Expression) extends BinaryExpression {
  def operator = ","

  def operandTypes = Seq(TypeArg(Left("a")), TypeArg(Left("b")))
  def resultType = TupleType(TypeArg(Left("a")), TypeArg(Left("b")))

  def optimize(operands: Seq[Expression]) = operands match {
    case Seq(Literal(x)   , Literal(y)) => Literal(TupleValue(x, y))
    case s                              => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withInfo(this)
}

case class FunCall(function: Either[String, FunDecl], arguments: Seq[Expression]) extends NAryExpression {
  def name = function.fold(identity, _.name)
  def operator = name
  def arity = arguments.size
  def operands = arguments

  def operandTypes = function.right.get.paramTypes
  def resultType = function.right.get.resultType

  def optimize(operands: Seq[Expression]) = operands match {
    case s if PredefinedFunctions.optimize.isDefinedAt((operator, s)) => PredefinedFunctions.optimize((operator, s))
    case s                                                            => rewrite(s)
  }

  def rewrite(ops: Seq[Expression]) = copy(arguments = ops).withInfo(this)

  override def checkValidity = PredefinedFunctions.checkValidity(operator, operands)
}