package nl.ru.cs.spl.ast

import nl.ru.cs.spl.{intermediate, SPLException}
import nl.ru.cs.spl.util._
import scala.collection.mutable
import nl.ru.cs.spl.intermediate._
import scala.Some
import nl.ru.cs.spl.intermediate.IRFunction


class Analyser(val errorHandler: ErrorHandler = new DefaultErrorHandler(), val optimize: Boolean = true) {
  private def collectTypeParams(tpe: Type): Set[TypeParam] = tpe match {
    case ListType(e)     => collectTypeParams(e)
    case TupleType(f, s) => collectTypeParams(f) ++ collectTypeParams(s)
    case t: TypeParam    => Set(t)
    case _               => Set()
  }

  private def unifyTypeArg(bindings: TypeBinding, name: Int, tpe: Type): Type = {
    def unify(t1: Type, t2: Type): Type = (t1, t2) match {
      case (a, b) if a == b                       => a
      case (ListType(x), ListType(y))             => ListType(unify(x, y))
      case (TupleType(a1, b1), TupleType(a2, b2)) => TupleType(unify(a1, a2), unify(b1, b2))
      case (TypeArg(Right(n)), t)                 => unifyTypeArg(bindings, n, t)
      case (t, TypeArg(Right(n)))                 => unifyTypeArg(bindings, n, t)
      case (a, b)                                 => throw new SPLException("Cannot unify types " + a + " and " + b)
    }

    val res = bindings(name).map(unify(_, tpe)).getOrElse(tpe)
    bindings(name) = res
    res
  }

  private def matchTypes(expected: Type, actual: Type, bindings: TypeBinding) {
    (expected, actual) match {
      case (t1,t2) if t1 == t2                  => // success

      case (WildcardType, _)                    => // success
      case (_, VoidType())                      => throw new SPLException("Found expression of type: Void; expected: " + expected)

      case (TypeArg(Right(n)), t)               => unifyTypeArg(bindings, n, t)
      case (t, TypeArg(Right(n)))               => unifyTypeArg(bindings, n, t)

      case (ListType(t1), ListType(t2))         => matchTypes(t1, t2, bindings)
      case (TupleType(a1,b1), TupleType(a2,b2)) => matchTypes(a1, a2, bindings); matchTypes(b1, b2, bindings)

      case _                                    => throw new SPLException("Found expression of type: " + actual + "; expected: " + expected)
    }
  }

  private def uniquifyTypeArgs(tpe: Type, names: mutable.Map[String, Int], bindings: TypeBinding): Type = tpe.transform {
    case TypeArg(Left(n)) => TypeArg(Right(names.getOrElseUpdate(n, bindings.getFreshName)))
  }

  private def substituteTypeArgs(bindings: TypeBinding, tpe: Type): Type = tpe.transform {
    case TypeArg(Right(n)) => bindings(n).getOrElse(WildcardType)
  }

  private def bindVarRef(env: Environment, varRef: VarRef) = varRef match {
    case v @ VarRef(Left(name))=>
      VarRef(Right(env.getVariable(env.resolveVariableName(name)).getOrElse(throw new SPLException("Undeclared variable: '" + name + "'", v)))).withInfo(v)
    case v => v
  }

  private def bindExpression(env: Environment, exp: Expression): Expression = exp.transform {
    case v: VarRef => bindVarRef(env, v)
    case f @ FunCall(Left(name), args) =>
      val decl = env.getFunction(name).getOrElse(throw new SPLException("Undeclared function: '" + name + "'", f))
      if (decl.paramTypes.size != args.size)
        throw new SPLException("Incorrect number of arguments for '" + name + "': " + args.size + "; expected: " + decl.paramTypes.size, f)
      FunCall(Right(decl), args).withInfo(f)
  }

  private def typeCheckExpression(env: Environment, bindings: TypeBinding, expectedType: Type, exp: Expression) {
    def recurse(expectedType: Type, exp: Expression) {
      val names = mutable.Map[String, Int]()
      val actualType = uniquifyTypeArgs(exp.resultType, names, bindings)
      try {
        matchTypes(expectedType, actualType, bindings)
      } catch {
        case e: SPLException => throw e.withPosition(exp)
      }

      exp.operandTypes.zip(exp.operands).foreach { case (t,e) =>
        recurse(uniquifyTypeArgs(t, names, bindings), e)
      }
    }

    recurse(expectedType, exp)
  }

  private def convertExpressionToInstructions(env: Environment, bindings: TypeBinding, exp: Expression): Seq[Instruction] = {
    def topLevelType(tpe: Type): Type = tpe match {
      case TypeArg(Right(n)) => topLevelType(bindings(n).getOrElse(WildcardType))
      case ListType(_)       => ListType(WildcardType)
      case TupleType(_, _)   => TupleType(WildcardType, WildcardType)
      case t                 => t
    }

    def recurse(exp: Expression): Seq[Instruction] = {
      val names = mutable.Map[String, Int]()

      val actualType = uniquifyTypeArgs(exp.resultType, names, bindings)
      exp.operandTypes.foreach { t =>
        uniquifyTypeArgs(t, names, bindings)
      }

      val typeInfo = names.map { case (name, index) =>
        name -> topLevelType(bindings(index).getOrElse(WildcardType))
      }.toMap

      //val tpe =
      exp.withTypeInfo(substituteTypeArgs(bindings, actualType))

      exp.operands.flatMap(recurse) :+ (exp match {
        case Literal(v)             => intermediate.LoadConstant(v)
        case VarRef(Right(v))       => intermediate.LoadVariable(v)
        case _: Add                 => intermediate.Add
        case _: Subtract            => intermediate.Subtract
        case _: Multiply            => intermediate.Multiply
        case _: Divide              => intermediate.Divide
        case _: Modulo              => intermediate.Modulo
        case _: Negate              => intermediate.Negate
        case _: And                 => intermediate.And
        case _: Or                  => intermediate.Or
        case _: Not                 => intermediate.Not
        case _: EQ                  => intermediate.EQ
        case _: NE                  => intermediate.NE
        case _: LT                  => intermediate.LT
        case _: LE                  => intermediate.LE
        case _: GT                  => intermediate.GT
        case _: GE                  => intermediate.GE
        case _: Cons                => intermediate.Cons(tuple = false, typeInfo)
        case _: TupleCons           => intermediate.Cons(tuple = true , typeInfo)
        case f: FunCall             => intermediate.Call(f.name, typeInfo)
      })
    }

    recurse(exp)
  }

  private def analyseExpression(env: Environment, expectedType: Type, exp: Expression): (Expression, Seq[Instruction]) = {
    val boundExpression = bindExpression(env, exp)
    val bindings = new TypeBinding
    typeCheckExpression(env, bindings, expectedType, boundExpression)
    bindings.resetNames()
    val instructions = convertExpressionToInstructions(env, bindings, boundExpression)
    (boundExpression, instructions)
  }

  private def checkVars[T >: VarDef](env: Environment, defs: Seq[T]): (Seq[VarDef], Seq[Instruction]) = {
    val (newVars, instructions) = defs.collect {
      case v @ VarDef(name, tpe, exp, global) =>
        collectTypeParams(tpe).foreach { t =>
          if (!env.isTypeVarDefined(t))
            errorHandler.error(new SPLException("Undeclared type variable '" + t.name + "' in variable '" + name + "'", t))
        }
        val (newExp, instructions) = try {
          analyseExpression(env, tpe, exp)
        } catch {
          case e: SPLException =>
            errorHandler.error(e)
            (exp, Seq())
        }


        val newName = env.declareVariable(v.declaration).getOrElse {
          errorHandler.error(new SPLException("Duplicate variable name: '" + name + "'", v)); name
        }
        (VarDef(newName, tpe, newExp, global).withInfo(v), instructions :+ StoreVariable(v.declaration, init = true))
    }.unzip
    (newVars, instructions.flatten)
  }


  //    def analyseCompound(env: Environment, compound: Compound, functionLevel: Boolean = false): (Statement, Boolean) = {
  //      val localEnv = if (functionLevel) env else env.create()
  //      val newVars = checkVars(localEnv, compound.variables)
  //
  //      val newBody = compound.statements.map(analyseStatement(localEnv))
  //      val l2 = newBody.dropWhile(!_._2)
  //      val terminates =
  //        if (l2.isEmpty) false
  //        else {
  //          l2.tail.headOption.foreach { case (s, _) => errorHandler.warning(new SPLException("Unreachable code.", s)) }
  //          true
  //        }
  //
  //      val flattenedBody = newBody.unzip._1.flatMap {
  //        case Compound(Seq(), b) => b
  //        case s                  => Seq(s)
  //      }
  //
  //      (Compound(newVars, flattenedBody).withInfo(compound), terminates)
  //    }

  //    def analyseStatement(env: Environment)(statement: Statement): (Statement, Boolean) = statement match {
  //      case c: Compound =>
  //        analyseCompound(env, c, functionLevel = false)
  //      case If(pred, cons, alt) =>
  //        val newExp = analyseExpression(env, Variable(env.declareTemporaryVariable(BoolType())), pred)
  //        val (newCons, consTerms) = analyseStatement(env)(cons)
  //        val newAltRes = alt.map(analyseStatement(env))
  //
  //        newExp match {
  //          case Literal(True)  =>
  //            errorHandler.warning(new SPLException("Condition always evaluates to true", statement))
  //
  //            (newCons.withInfo(statement), consTerms)
  //          case Literal(False) =>
  //            errorHandler.warning(new SPLException("Condition always evaluates to false", statement))
  //
  //            newAltRes match  {
  //              case Some((newAlt, altTerms)) => (newAlt.withInfo(statement), altTerms)
  //              case None => (Compound.empty.withInfo(statement), false)
  //            }
  //          case _ =>
  //            (If(newExp, newCons, newAltRes.map(_._1)).withInfo(statement), consTerms && newAltRes.map(_._2).getOrElse(false))
  //        }
  //
  //      case While(pred, cons) =>
  //        val newExp = analyseExpression(env, Variable(env.declareTemporaryVariable(BoolType())), pred)
  //        val (newCons, terms) = analyseStatement(env)(cons)
  //
  //        newExp match {
  //          case Literal(True)  =>
  //            if (terms) {
  //              errorHandler.warning(new SPLException("Loop is executed only once", statement))
  //              (newCons, true)
  //            }
  //
  //            (While(newExp, newCons).withInfo(statement), true)
  //          case Literal(False) =>
  //            errorHandler.warning(new SPLException("Condition always evaluates to false; loop is never executed", statement))
  //            (Compound.empty.withInfo(statement), false)
  //          case _              =>
  //            if (terms) {
  //              errorHandler.warning(new SPLException("Loop is executed at most once; use 'if' instead", statement))
  //              (If(newExp, newCons, None).withInfo(statement), terms)
  //            } else
  //              (While(newExp, newCons).withInfo(statement), terms)
  //        }
  //
  //      case Assignment(name, value) =>
  //        val target = env.getVariable(env.resolveVariableName(name)).map(Variable(_)).getOrElse {
  //          errorHandler.error(new SPLException("Undeclared variable: '" + name + "'", statement))
  //          Discard
  //        }
  //        (Assignment(name, analyseExpression(env, target, value)).withInfo(statement), false)
  //
  //      case f @ FunCall(name, args, true) =>
  //        (analyseExpression(env, Discard, f).asInstanceOf[FunCall], false)
  //
  //      case Return(exp) =>
  //        val retDecl = env.getVariable(env.resolveVariableName("$result"))
  //        if (retDecl.isDefined) {
  //          if (exp.isDefined)
  //            return (Return(Some(analyseExpression(env, retDecl.map(Variable(_)).getOrElse(Discard) , exp.get))).withInfo(statement), true)
  //
  //          errorHandler.error(new SPLException("Return statement requires a return value", statement))
  //        } else if (exp.isDefined) {
  //          errorHandler.error(new SPLException("Returning result from a procedure", statement))
  //        }
  //        (Return(None).withInfo(statement), true)
  //    }

  private def analyseFunction(env: Environment, f: FunDef): IRFunction = {
    val exitBlock = new ExitBlock

    val blocks = mutable.ArrayBuffer[NormalBlock]()

    def createBlock() = {
      val block = new NormalBlock(blocks.size.toString)
      blocks += block
      block
    }

    def analyseCompound(env: Environment, compound: Compound, block: NormalBlock): (Statement, NormalBlock) = {
      val (newVars, instrVars) = checkVars(env, compound.variables)
      block.instructions ++= instrVars
      val (newBody, newBlock) = compound.statements.foldLeft((Seq[Statement](), block)) { case ((body, prevBlock), s) =>
        try {
          val (stmt, newBlock) = analyseStatement(env, s, prevBlock)
          (body :+ stmt, newBlock)
        } catch {
          case e: SPLException =>
            errorHandler.error(e)
            (body, prevBlock)
        }
      }
      (Compound(newVars, newBody).withInfo(compound), newBlock)
    }

    def analyseStatement(env: Environment, statement: Statement, localEntryBlock: NormalBlock): (Statement, NormalBlock) = statement match {
      case c: Compound =>
        analyseCompound(env.create, c, localEntryBlock)
      case If(pred, cons, optAlt) =>
        val (newExp, instrExp) = analyseExpression(env, BoolType(), pred)
        localEntryBlock.instructions ++= instrExp

        val consEntryBlock = createBlock()
        val (newCons, consExitBlock) = analyseStatement(env, cons, consEntryBlock)

        val optAltRes = optAlt.map { alt =>
          val altEntryBlock = createBlock()
          val (newAlt, altExitBlock) = analyseStatement(env, alt, altEntryBlock)
          (newAlt, altEntryBlock, altExitBlock)
        }

        val localExitBlock = createBlock()
        consExitBlock.linkTo(localExitBlock)
        optAltRes.foreach(_._3.linkTo(localExitBlock))
        localEntryBlock.linkTo(consEntryBlock, optAltRes.fold(localExitBlock)(_._2))

        (If(newExp, newCons, optAltRes.map(_._1)).withInfo(statement), localExitBlock)
      case While(pred, body) =>
        val (newExp, instrExp) = analyseExpression(env, BoolType(), pred)
        val headerBlock = createBlock()
        headerBlock.instructions ++= instrExp
        localEntryBlock.linkTo(headerBlock)

        val bodyEntryBlock = createBlock()

        val (newCons, bodyExitBlock) = analyseStatement(env, body, bodyEntryBlock)
        bodyExitBlock.linkTo(headerBlock)

        val localExitBlock = createBlock()
        headerBlock.linkTo(bodyEntryBlock, localExitBlock)

        (While(newExp, newCons).withInfo(statement), localExitBlock)
      case Assignment(varRef, value) =>
        val newVar @ VarRef(Right(decl)) = bindVarRef(env, varRef)
        val (newExp, instrExp) = analyseExpression(env, decl.tpe, value)
        localEntryBlock.instructions ++= instrExp
        localEntryBlock.instructions += StoreVariable(decl, init = false)

        (Assignment(newVar, newExp).withInfo(statement), localEntryBlock)
      case ProcCall(fun) =>
        val (newExp, instrExp) = analyseExpression(env, WildcardType, fun)
        localEntryBlock.instructions ++= instrExp
        val newFun = newExp.asInstanceOf[FunCall]
        if (!newFun.resultType.isVoid)
          localEntryBlock.instructions += Discard(newFun.resultType)

        (ProcCall(newExp.asInstanceOf[FunCall]).withInfo(statement), localEntryBlock)
      case Return(optExp) =>
        val retDecl = env.getVariable(env.resolveVariableName("$result"))

        if (retDecl.isDefined) {
          if (optExp.isDefined) {
            val (newExp, instrExp) = analyseExpression(env, retDecl.get.tpe, optExp.get)
            localEntryBlock.instructions ++= instrExp
            localEntryBlock.instructions += StoreVariable(retDecl.get, init = true)
            localEntryBlock.linkTo(exitBlock)

            (Return(Some(newExp)).withInfo(statement), createBlock())
          } else {
            throw new SPLException("Return statement requires a return value", statement)
          }
        } else if (optExp.isDefined) {
          throw new SPLException("Returning result from a procedure", statement)
        } else {
          localEntryBlock.linkTo(exitBlock)
          (Return(None).withInfo(statement), createBlock())
        }
    }

    val funEnv = env.create
    val newParams = f.parameters.map { p =>
      collectTypeParams(p.tpe).foreach(funEnv.declareTypeVar)

      funEnv.declareVariable(p.declaration).map(Param(_, p.tpe).withInfo(p)).getOrElse {
        errorHandler.error(new SPLException("Duplicate parameter name: '" + p.name + "'", p))
        p
      }
    }
    if (!f.resultType.isVoid)
      funEnv.declareVariable(VarDecl("$result", f.resultType, VarType.Result))

    val entryBlock = createBlock()
    val (newBody, lastBlock) = analyseCompound(funEnv, f.body, entryBlock)

    if (f.resultType.isVoid)
      lastBlock.linkTo(exitBlock)

    val newBlocks = blocks.flatMap { block =>
      if (block.instructions.isEmpty) {
        block.remove()
        Seq()
      } else {
        Seq(block)
      }
    }

    IRFunction(FunDef(f.name, f.resultType, newParams, newBody.asInstanceOf[Compound]).withInfo(f), funEnv, new EntryBlock(entryBlock) +: newBlocks :+ exitBlock)
  }

  def analyse(program: Program): IRProgram = {
    val env = Environment.create

    program.definitions.collect {
      case f @ FunDef(name, _, _, _) =>
        if (!env.declareFunction(f.declaration))
          errorHandler.error(new SPLException("Duplicate function name: '" + name + "'", f))
    }

    val (newVars, instrVar) = checkVars(env, program.definitions)

    val irFuns = program.definitions.collect {
      case f: FunDef => analyseFunction(env, f)
    }

    val newProgram = Program(newVars ++ irFuns.map(_.ast)).withInfo(program)

    IRProgram(env, newProgram, irFuns, instrVar)
  }
}



class TypeBinding {
  private val buffer = mutable.ArrayBuffer[Option[Type]]()
  private var key = 0

  def apply(name: Int) = buffer(name)

  def update(name: Int, tpe: Type) {
    buffer(name) = Some(tpe)
  }

  def resetNames() {
    key = 0
  }

  def getFreshName: Int = {
    val res = key
    key += 1
    buffer += None
    res
  }
}