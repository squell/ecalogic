package nl.ru.cs.spl.util

import java.io.PrintWriter
import nl.ru.cs.spl.intermediate._
import nl.ru.cs.spl.intermediate.EntryBlock
import nl.ru.cs.spl.intermediate.IRFunction
import org.apache.commons.lang3.StringEscapeUtils
import nl.ru.cs.spl.ast._
import nl.ru.cs.spl.intermediate.LT
import nl.ru.cs.spl.intermediate.Or
import nl.ru.cs.spl.intermediate.LoadConstant
import nl.ru.cs.spl.ast.IntType
import nl.ru.cs.spl.intermediate.Multiply
import nl.ru.cs.spl.intermediate.Not
import nl.ru.cs.spl.ast.BoolType
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
import nl.ru.cs.spl.intermediate.Modulo
import nl.ru.cs.spl.intermediate.NE
import nl.ru.cs.spl.ast.ListType
import nl.ru.cs.spl.intermediate.Add
import nl.ru.cs.spl.intermediate.Subtract
import nl.ru.cs.spl.intermediate.LE
import nl.ru.cs.spl.ast.TupleType
import nl.ru.cs.spl.intermediate.Discard
import nl.ru.cs.spl.intermediate.Negate
import nl.ru.cs.spl.intermediate.IRFunction

class IRDotPrinter(output: PrintWriter = new PrintWriter(Console.out)) {
  private def println(s: String) {
    output.println(s)
  }

  def instrString(instruction: Instruction) = instruction match {
    case LoadVariable(from)   => s"<td align='left'><b>load</b></td><td align='left'>${from.name}</td>"
    case LoadConstant(from)   => s"<td align='left'><b>load</b></td><td align='left'>$from</td>"
    case StoreVariable(to, _) => s"<td align='left'><b>store</b></td><td align='left'>${to.name}</td>"
    case Discard(tpe)         => "<td align='left'><b>discard</b></td>"
    case Add                  => "<td align='left'><b>add</b></td>"
    case Subtract             => "<td align='left'><b>subtract</b></td>"
    case Multiply             => "<td align='left'><b>multiply</b></td>"
    case Divide               => "<td align='left'><b>divide</b></td>"
    case Modulo               => "<td align='left'><b>modulo</b></td>"
    case Negate               => "<td align='left'><b>negate</b></td>"
    case And                  => "<td align='left'><b>and</b></td>"
    case Or                   => "<td align='left'><b>or</b></td>"
    case Not                  => "<td align='left'><b>not</b></td>"
    case EQ                   => "<td align='left'><b>eq</b></td>"
    case NE                   => "<td align='left'><b>ne</b></td>"
    case LT                   => "<td align='left'><b>lt</b></td>"
    case LE                   => "<td align='left'><b>le</b></td>"
    case GT                   => "<td align='left'><b>gt</b></td>"
    case GE                   => "<td align='left'><b>ge</b></td>"
    case Cons(_, info)        => "<td align='left'><b>cons</b></td><td></td>" + info.map(m => s"<td align='left'><i>${m._1}</i>=${typeString(m._2)}</td>)}")
    case Call(name, info)     => s"<td align='left'><b>call</b></td><td align='left'>$name</td>" + info.map(m => s"<td align='left'><i>${m._1}</i>=${typeString(m._2)}</td>)}")
  }

  def print(name: String, program: IRProgram) {
    println(s"digraph $name {")
    println("  graph [fontname=Courier]")
    println("  node [fontname=Courier, margin=0]")
    println("  edge [fontname=Courier]")
    program.functions.foreach(print)
    println("}")
  }

  def typeString(tpe: Type): String = tpe match {
    case ListType(x)     => s"[${typeString(x)}&#93;"
    case TupleType(a, b) => s"(${typeString(a)}, ${typeString(b)})"
    case WildcardType    => "<i>*</i>"
    case p: TypeParam    => s"<i>$p</i>"
    case t               => s"<b>$t</b>"
  }

  def print(function: IRFunction) {
    val funName = function.name
    println(s"  subgraph cluster_$funName {")
    println(s"    label=<${typeString(function.ast.resultType)} $funName(${function.ast.parameters.map(p => typeString(p.tpe) + " " + p.name).mkString(", ")})>")
    val varString = function.env.collectVariables(recursive = true).map(v => s"<tr><td align='left'>${typeString(v.tpe)}</td><td align='left'>${v.name}</td></tr>").mkString
    println(s"    ${funName}__vars [label=<<table>$varString</table>>, shape=none]")

    function.blocks.foreach { case from =>
      val attr = from match {
        case _: EntryBlock  => "[label=<&lt;<b>entry</b>&gt;>, shape=Mdiamond]"
        case _: ExitBlock   => "[label=<&lt;<b>exit</b>&gt;>, shape=Msquare]"
        case b: NormalBlock =>
          val instr = b.instructions.map(i => s"<tr>${instrString(i)}</tr>").mkString
          val branch = if (b.isBranch) s"<hr/><tr><td align='left'><b>condjump</b></td></tr>" else ""

          s"[label=<<table cellborder='0'>$instr$branch</table>>, shape=none]"
      }
      println(s"    ${from.createName(funName)} $attr;")

      from match {
        case b: NormalBlock if b.isBranch =>
          println(s"    ${from.createName(funName)}:s -> ${b.successor(condition = true).createName(funName)}:n [label=<<b>True</b>>];")
          println(s"    ${from.createName(funName)}:s -> ${b.successor(condition = false).createName(funName)}:n [label=<<b>False</b>>];")
        case b =>
          b.successors.foreach(s =>  println(s"    ${from.createName(funName)}:s -> ${s.createName(funName)}:n;"))
      }
    }
    println("  }")
  }

  def flush() {
    output.flush()
  }

  def close() {
    output.close()
  }
}
