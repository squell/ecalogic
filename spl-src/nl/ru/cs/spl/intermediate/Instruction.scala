package nl.ru.cs.spl.intermediate

import nl.ru.cs.spl.ast._

sealed trait Instruction

case class LoadVariable(from: VarDecl) extends Instruction
case class LoadConstant(from: Value) extends Instruction

case class StoreVariable(to: VarDecl, init: Boolean) extends Instruction

case class Discard(tpe: Type) extends Instruction

case object Add extends Instruction
case object Subtract extends Instruction
case object Multiply extends Instruction
case object Divide extends Instruction
case object Modulo extends Instruction
case object Negate extends Instruction
case object And extends Instruction
case object Or extends Instruction
case object Not extends Instruction
case object EQ extends Instruction
case object NE extends Instruction
case object LT extends Instruction
case object LE extends Instruction
case object GT extends Instruction
case object GE extends Instruction
case class Cons(tuple: Boolean, typeInfo: Map[String, Type]) extends Instruction

case class Call(name: String, typeInfo: Map[String, Type]) extends Instruction





