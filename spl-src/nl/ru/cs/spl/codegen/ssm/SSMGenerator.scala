package nl.ru.cs.spl.codegen.ssm

import java.io.PrintWriter
import nl.ru.cs.spl.ast._

import nl.ru.cs.spl.intermediate._
import scala.collection.mutable
import nl.ru.cs.spl.intermediate.LT
import nl.ru.cs.spl.intermediate.Or
import nl.ru.cs.spl.intermediate.LoadConstant
import nl.ru.cs.spl.ast.IntType
import nl.ru.cs.spl.intermediate.Multiply
import nl.ru.cs.spl.intermediate.Not
import nl.ru.cs.spl.ast.BoolType
import nl.ru.cs.spl.ast.VarDecl
import nl.ru.cs.spl.intermediate.Cons
import nl.ru.cs.spl.intermediate.Divide
import nl.ru.cs.spl.intermediate.IRProgram
import nl.ru.cs.spl.intermediate.GE
import nl.ru.cs.spl.intermediate.EQ
import nl.ru.cs.spl.intermediate.GT
import nl.ru.cs.spl.intermediate.StoreVariable
import nl.ru.cs.spl.intermediate.Call
import nl.ru.cs.spl.intermediate.And
import nl.ru.cs.spl.intermediate.LoadVariable
import nl.ru.cs.spl.ast.IntValue
import nl.ru.cs.spl.intermediate.Modulo
import nl.ru.cs.spl.intermediate.NE
import nl.ru.cs.spl.ast.ListType
import nl.ru.cs.spl.intermediate.Add
import nl.ru.cs.spl.intermediate.Subtract
import nl.ru.cs.spl.intermediate.LE
import nl.ru.cs.spl.ast.TupleType
import nl.ru.cs.spl.intermediate.Discard
import nl.ru.cs.spl.intermediate.Negate
import nl.ru.cs.spl.ast.FunDecl
import nl.ru.cs.spl.intermediate.IRFunction
import nl.ru.cs.spl.SPLException

class SSMGenerator(output: PrintWriter = new PrintWriter(Console.out), referenceCounting: Boolean = false) {
  private def println() {
    output.println()
  }

  private def println(s: String) {
    output.println(s)
  }

  private def printInstr(s: String) {
    println("    " + s)
  }


  private def typeString(t: Type): String = t match {
    case _: IntType   => "I"
    case _: TupleType => "T"
    case _: BoolType  => "B"
    case _: ListType  => "L"
  }

  private def print(instruction: Instruction,
                    typeArgs: Map[String, Type],
                    constants: mutable.Buffer[Value],
                    globals: IndexedSeq[VarDecl],
                    locals: IndexedSeq[VarDecl],
                    params: IndexedSeq[VarDecl],
                    funQueue: mutable.Queue[(String, Map[String, Type])],
                    funClosed: mutable.Set[(String, Map[String, Type])]) {
    def typeCode(t: Type): Int = t match {
      case _: IntType   => 0
      case _: TupleType => 1
      case _: BoolType  => 2
      case _: ListType  => 3
    }

    instruction match {
      case LoadConstant(IntValue(x)) =>
        if (x > Int.MaxValue || x < Int.MinValue) {
          throw new SPLException(s"Integer constant out of bounds: $x")
        }
        printInstr(s"ldc $x")
      case LoadConstant(True) =>
        printInstr("ldc 0xFFFFFFFF")
      case LoadConstant(False) | LoadConstant(NilValue) =>
        printInstr("ldc 0x00000000")
      case LoadConstant(v) =>
        printInstr("ldr R6")
        var offset = constants.indexOf(v)
        if (offset == -1) {
          offset = constants.size
          constants += v
        }
        printInstr(s"lda ${globals.size + offset}")
      case LoadVariable(from) =>
        from.varType match {
          case VarType.Global =>
            printInstr("ldr R6")
            printInstr(s"lda ${globals.indexOf(from)}")
          case VarType.Param =>
            printInstr(s"ldl ${-params.indexOf(from) - 2}")
          case VarType.Local =>
            printInstr(s"ldl ${locals.indexOf(from) + 1}")
        }
        if (referenceCounting && from.tpe.isComposite) {
          printInstr("lds 0")
          printInstr("bsr __addref")
        }
      case StoreVariable(to, init) =>
        if (to.varType == VarType.Result) {
          // Leave on stack
        } else if (to.tpe.isComposite && !init) {
          to.varType match {
            case VarType.Global =>
              printInstr("ldr R6")
              printInstr(s"ldaa ${globals.indexOf(to)}")
            case VarType.Param =>
              printInstr(s"ldla ${-params.indexOf(to) - 2}")
            case VarType.Local =>
              printInstr(s"ldla ${locals.indexOf(to) + 1}")
          }
          if (referenceCounting) {
            printInstr("lds 0")
            printInstr("lda 0")
            printInstr("bsr __release")
          }
          printInstr("sta 0")
        } else {
          to.varType match {
            case VarType.Global =>
              printInstr("ldr R6")
              printInstr(s"sta ${globals.indexOf(to)}")
            case VarType.Param =>
              printInstr(s"stl ${-params.indexOf(to) - 2}")
            case VarType.Local =>
              printInstr(s"stl ${locals.indexOf(to) + 1}")
          }
        }
      case Discard(tpe: Type) =>
        if (referenceCounting && tpe.isComposite)
          printInstr("bsr __release")
        else
          printInstr("ajs -1")
      case Add => printInstr("add")
      case Subtract => printInstr("sub")
      case Multiply => printInstr("mul")
      case Divide => printInstr("div")
      case Modulo => printInstr("mod")
      case Negate => printInstr("neg")
      case And => printInstr("and")
      case Or => printInstr("or")
      case Not => printInstr("not")
      case EQ => printInstr("eq")
      case NE => printInstr("ne")
      case LT => printInstr("lt")
      case LE => printInstr("le")
      case GT => printInstr("gt")
      case GE => printInstr("ge")
      case Cons(tuple, typeInfo) =>
        val boundTypeInfo = typeInfo.mapValues {
          case TypeParam(n) => typeArgs(n)
          case t            => t
        }
        printInstr("bsr __alloc")
        printInstr("lds 0")
        printInstr("str R5")
        if (tuple)
          printInstr(f"ldc ${1 | typeCode(boundTypeInfo("a")) << 28 | typeCode(boundTypeInfo("b")) << 24}%#08x")
        else
          printInstr(f"ldc ${1 | typeCode(boundTypeInfo("t")) << 28 | typeCode(ListType(WildcardType)) << 24}%#08x")
        printInstr("lds -1")
        printInstr("sta 0")
        printInstr("stma 1 2")
        printInstr("ldr R5")
      case Call(name, _) if PredefinedFunctions.isPredefined(name) && name != "print" =>
        printInstr(s"bsr $name")
      case Call(name, typeInfo) =>
        val boundTypeInfo = typeInfo.mapValues {
          case WildcardType => IntType()
          case TypeParam(n) => typeArgs(n)
          case t            => t
        }
        if (!PredefinedFunctions.isPredefined(name) && !funClosed((name, boundTypeInfo))) {
          funClosed += ((name, boundTypeInfo))
          funQueue += ((name, boundTypeInfo))
        }
        printInstr(s"bsr $name${if (PredefinedFunctions.isPredefined(name) && name != "print" || boundTypeInfo.isEmpty) "" else "__" + boundTypeInfo.values.map(typeString).mkString}")
    }
  }

  private def print(function: IRFunction,
                    typeArgs: Map[String, Type],
                    constants: mutable.Buffer[Value],
                    globals: IndexedSeq[VarDecl],
                    funQueue: mutable.Queue[(String, Map[String, Type])],
                    funClosed: mutable.Set[(String, Map[String, Type])]) {
    val params = function.ast.parameters.map(_.declaration).reverse.toIndexedSeq
    val locals = function.env.collectVariables(recursive = true).filter(_.varType == VarType.Local).toIndexedSeq
    val funName = function.name + (if (typeArgs.isEmpty) "" else "__" + typeArgs.values.map(typeString).mkString)

    function.blocks.zip(function.blocks.tail :+ null).foreach { case (block, next) =>
      println(s"${block.createName(funName)}:")
      block match {
        case b: EntryBlock =>
          if (!function.ast.resultType.isVoid && params.isEmpty)
            printInstr("lds 0")
          printInstr(s"link 0")
          locals.foreach(_ => printInstr("ldc 0"))
        case b: ExitBlock =>
          if (referenceCounting) {
            params.filter(_.tpe.isComposite).zipWithIndex.foreach { case (_, idx) =>
              printInstr(s"ldl ${-idx - 2}")
              printInstr("bsr __release")
            }
            locals.filter(_.tpe.isComposite).zipWithIndex.foreach { case (_, idx) =>
              printInstr(s"ldl ${idx + 1}")
              printInstr("bsr __release")
            }
          }
          if (!function.ast.resultType.isVoid) {
            printInstr(s"stl ${Math.min(-params.size - 1, -2)}")
          }
          printInstr("unlink")

          val results = if (function.ast.resultType.isVoid) 0 else 1
          if (params.size > results) {
            printInstr(s"sts ${results - params.size}")
            if (params.size > results + 1)
              printInstr(s"ajs ${results + 1 - params.size}")
          }
          printInstr("ret")
        case b: NormalBlock =>
          b.instructions.foreach(print(_, typeArgs, constants, globals, locals, params, funQueue, funClosed))
          if (b.isBranch) {
            if (b.successor(condition = true) == next) {
              printInstr(s"brf ${b.successor(condition = false).createName(funName)}")
            } else {
              printInstr(s"brt ${b.successor(condition = true).createName(funName)}")

              if (b.successor(condition = false) != next)
                printInstr(s"bra ${b.successor(condition = false).createName(funName)}")
            }
          } else {
            if (b.successor != next) {
              printInstr(s"bra ${b.successor.createName(funName)}")
            }
          }
      }
    }
    println()
  }

  def print(program: IRProgram) {
    val constants = mutable.Buffer[Value]()
    val globals = program.env.collectVariables(recursive = false).toIndexedSeq

    val funMap = program.functions.map(f => f.name -> f).toMap
    val functions = mutable.Queue[(String, Map[String, Type])]()
    val closed = mutable.Set[(String, Map[String, Type])]()

    val main = funMap.getOrElse("main", throw new SPLException("Procedure 'main' is missing"))
    if (main.ast.resultType != VoidType() || !main.ast.parameters.isEmpty)
      throw new SPLException("Procedure 'main' has incorrect signature")

    // TODO: Fix constant pool generation
    println("#include \"system\"")
    println()
    println("__init:")
    if (!globals.isEmpty) {
      printInstr("ldr HP")
      printInstr(s"ldaa ${globals.size}")
      printInstr("str HP")
    }
    program.initialiser.foreach(print(_, Map.empty, constants, globals, IndexedSeq.empty, IndexedSeq.empty, functions, closed))
    //    constants.foreach {
    //      case TupleValue(l, r) =>
    //
    //
    //    }
    printInstr("ret")
    println()


    functions += (("main", Map.empty))
    do  {
      val (f, m) = functions.dequeue()
      closed += ((f, m))
      print(funMap(f), m, constants, globals, functions, closed)
    } while (!functions.isEmpty)
  }
}
