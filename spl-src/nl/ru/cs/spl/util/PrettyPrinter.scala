package nl.ru.cs.spl.util

import scala.io.Source

import java.io.PrintWriter

import nl.ru.cs.spl.ast._
import nl.ru.cs.spl.parser.Parser

class PrettyPrinter(output: PrintWriter = new PrintWriter(Console.out),
                    indentString: String = "    ",
                    private var indentLevel: Int = 0,
                    private var startOfLine: Boolean = true,
                    printTypeInfo: Boolean = false) {

  require(indentString != null, "indentString == null")
  require(output != null      , "output == null")
  require(indentLevel >= 0    , "indentLevel < 0")

  protected def indent(statement: => Unit) {
    indentLevel += 1
    statement
    indentLevel -= 1
  }

  protected def optIndent(node: ASTNode)(statement: => Unit) {
    node match {
      case Compound(_, _) => statement
      case _ => indent(statement)
    }
  }

  protected def print(nodes: Seq[ASTNode], left: String, separator: String, right: String) = nodes.toList match {
    case Nil =>
    case n :: Nil =>
      this << left << n << right
    case n :: ns =>
      this << left << n
      for (n <- ns) {
        this << separator << n
      }
      this << right
  }

  protected def print(nodes: Seq[ASTNode], separator: String) {
    print(nodes, "", separator, "")
  }

  def print(str: String) {
    val builder = new StringBuilder

    for (c <- str) {
      if (startOfLine) {
        builder.append(List.fill(indentLevel)(indentString).mkString)
        startOfLine = false
      }

      builder += c
      if (c == '\n') {
        startOfLine = true
      }
    }
    output.print(builder)
  }

  def print(node: ASTNode) {
    node match {
      case ErrorNode(e) =>
        this << "<error" + e.map(" : " + _.getMessage + ">").getOrElse(">")

      case Program(ds) =>
        print(ds, "\n\n")
      case FunDef(n, t, ps, b) =>
        this << t << " " << n << "("
        print(ps, ", ")
        this << ")" << "\n" << b
      case VarDef(n, t, v, _) =>
        this << t << " " << n << " = " << v << ";"
      case Param(n, t) =>
        this << t << " " << n


      case t: Type =>
        this << t.toString


      case Compound(vs, ss) =>
        this << "{" << "\n"
        indent {
          print(vs ++ ss, "\n")
        }
        this << "\n" << "}"
      case If(p, c, oa) =>
        this << "if (" << p << ")" << "\n"
        optIndent(c) {
          this << c
        }
        for (a <- oa) {
          this << "\n" << "else" << "\n"
          optIndent(a) {
            this << a
          }
        }
      case While(p, c) =>
        this << "while (" << p << ")" << "\n"
        optIndent(c) {
          this << c
        }
      case Assignment(n, e) =>
        this << n.name << " = " << e << ";"

      case ProcCall(f) =>
        this << f << ";"

      case Return(oe) =>
        this << "return"

        for (e <- oe) {
          this << " " << e
        }
        this << ";"

      case e: Expression =>
        if (printTypeInfo) e.typeInfo.filter(_ != VoidType()).foreach(this << _ << "@")
        e match {
          case Literal(v) =>
            this << v.toString
          case v: VarRef =>
            this << v.name
          case TupleCons(l, r) =>
            this << "(" << l << "," << r << ")"
          case f @ FunCall(_, ps) =>
            this << f.name << "("
            print(ps, ", ")
            this << ")"
          case e: BinaryExpression =>
            this << "(" << e.left << " " << e.operator << " " << e.right << ")"
          case e: UnaryExpression =>
            if (printTypeInfo) this << "(" << e.operator << e.operand << ")"
            else this << e.operator << e.operand
        }
    }
  }

  def println(str: String) {
    print(str)
    println()
  }

  def println() {
    print("\n")
  }

  def println(node: ASTNode) {
    print(node)
    println()
  }

  def <<(str: String): PrettyPrinter = {
    print(str)
    this
  }

  def <<(node: ASTNode): PrettyPrinter = {
    print(node)
    this
  }

  def flush() {
    output.flush()
  }

  def close() {
    output.close()
  }

}

object PrettyPrinter {

  def main(args: Array[String]) {
    val arg = if (args.length == 0) None else Some(args(0))
    val source = Source.fromFile(arg getOrElse "Example0_Factorial.spl").mkString
    val parser = new Parser(source)
    val program = parser.program()
    val printer = new PrettyPrinter()

    println("============================================")
    println(program)
    printer.println("============================================")
    printer.println(program)
    printer.println("============================================")
    printer.flush()
  }

}