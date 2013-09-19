package nl.ru.cs.spl.scala

import scala.io.Source

import java.io.{File, PrintWriter}

import nl.ru.cs.spl.ast._
import nl.ru.cs.spl.util.PrettyPrinter
import nl.ru.cs.spl.parser.Parser
import nl.ru.cs.spl.SPLException
import nl.ru.cs.spl.scala.{Runtime => SPLRuntime}

class ScalaPrinter(output: PrintWriter = new PrintWriter(Console.out)) extends PrettyPrinter(output, "  ") {

  def printPackage(packageName: String) {
    this << "package " << packageName << "\n\n"
  }

  def printObject(name: String, members: Program) {
    this << "object " << name << " extends " << classOf[SPLRuntime].getName << "\n" << "{" << "\n\n"
    indent {
      this << members
    }
    this << "\n\n" << "}"
  }

  private def collectTypeParams(tpe: Type): Set[TypeParam] = tpe match {
    case ListType(e)     => collectTypeParams(e)
    case TupleType(f, s) => collectTypeParams(f) ++ collectTypeParams(s)
    case t: TypeParam    => Set(t)
    case _               => Set()
  }

//  private def replaceVariables(node: ASTNode, vs: Set[String]): ASTNode = {
//    def walkTree(node: ASTNode): ASTNode = node match {
//      case Assignment(n, v) if (vs.contains(n)) => Assignment("$" + n, walkTree(v).asInstanceOf[Expression])
//      case VarRef(n) if (vs.contains(n))        => VarRef("$" + n)
//      case n                                    => n.map(walkTree)
//    }
//    walkTree(node)
//  }

  private def findAssignedParameters(node: ASTNode, ps: Seq[Param]): Seq[String] = node match {
    case Assignment(n, v) if (ps.exists(_.name == n)) => Seq(n.name)
    case s: Statement                                 => s.children.flatMap(findAssignedParameters(_, ps))
    case _                                            => Seq()
  }

  override def print(node: ASTNode) {
    node match {
      case ErrorNode(e) =>
        throw new SPLException("AST contains error(s).", None, e)

      case Program(ds) =>
        print(ds, "\n\n")
// FIXME
//      case FunDef(n, t, ps, Compound(vs, ss)) =>
//        val assignedParams = ss.flatMap(findAssignedParameters(_, ps)).toSet
//        val nss = ss.map(replaceVariables(_, assignedParams))
//        val nvs = assignedParams.toSeq.map(n => VarDef("$" + n, ps.find(_.name == n).get.tpe, VarRef(n)))
//
//        this << "def " << n
//        val typeParams = ps.map((p => collectTypeParams(p.tpe))).foldLeft(collectTypeParams(t))(_ ++ _)
//
//        print(typeParams.toSeq, "[", ", ", "]")
//        this << "("
//        print(ps, ", ")
//        this << "): " << t << " =" << "\n" << "{" << "\n"
//        indent {
//          print(vs ++ nvs ++ nss, "\n")
//        }
//        this << "\n" << "}"
      case VarDef(n, t, v, _) =>
        this << "var " << n << ": " << t << " = " << v << ";"
      case Param(n, t) =>
        this << n << ": " << t


      case BoolType() =>
        this << "Boolean"
      case IntType() =>
        this << "Long"
      case VoidType() =>
        this << "Unit"
      case ListType(e) =>
        this << "List[" << e << "]"
      case TupleType(f, s) =>
        this << "(" << f << "," << s << ")"
      case TypeParam(n) =>
        this << n

      case Literal(NilValue) =>
        this << "Nil"
      case Literal(True) =>
        this << "true"
      case Literal(False) =>
        this << "false"
      case Literal(IntValue(v)) =>
        this << v.toString << "L"

      case Cons(h, t) =>
        this << h << " :: " << t

      case n =>
        super.print(n)
    }
  }

}

object ScalaPrinter {
  def main(args: Array[String]) {
    val arg = if (args.length == 0) None else Some(args(0))
    val file = new File(arg getOrElse "Example0_Factorial.spl").getCanonicalFile
    val source = Source.fromFile(file).mkString
    val parser = new Parser(source)
    val printer = new ScalaPrinter()
    printer.printObject(file.getName.takeWhile(_ != '.'), parser.program())
    printer.println()
    printer.flush()
  }
}