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
package util

import parser.Parser
import nl.ru.cs.ecalogic.ast._

import scala.io.Source

import java.io.File

/**
 * @author Dorus Peelen
 */
class PrettyPrinter(program: Program, eh: ErrorHandler = new DefaultErrorHandler()) {

  /** Print the prog */
  def printProg() {
    def printPart(node: ASTNode, depth: Int) {

      node match {
        case If(pred, thenPart, elsePart) =>
          print("If ")
          printPart(pred, depth)
          print(" then")
          printDepthln(1)
          printPart(thenPart, depth + 1)
          printDepthln()
          print(" else ")
          printPart(elsePart, depth + 1)
          printDepthln()
          print("end if")
        case While(pred, rf, consq) =>
          print("while ")
          printPart(pred, depth)
          rf.foreach { rf =>
            print(" bound ")
            printPart(rf, depth)
          }
          print(" do")
          printDepthln(1)
          printPart(consq, depth + 1)
          printDepthln()
          print("end while")
        case Composition(stms) =>
          var fst = true; for (stmt <- stms) {
            if (!fst) {
              print(";")
              printDepthln()
            }
            fst = false
            printPart(stmt, depth)
          }
        case Assignment(ident, expr) =>
          print(ident + " := ")
          printPart(expr, depth)
        case Skip() =>
          print("skip")
        case FunCall(fun, args) =>
          if (fun.isPrefixed) print(fun.prefix.get + "::") else ""
          print(fun.name)
          printParams(args)
        case VarRef(ident) => print(ident)
        case e: BinaryExpression =>
          printPart(e.left, depth)
          print(" " + e.operatorName + " ")
          printPart(e.right, depth)
          // TODO Fix braces
        case e: UnaryExpression =>
          printPart(e.operand, depth)
          e.operands.foreach(printPart(_, depth))
        case FunDef(name, params, body) =>
          print("function " + name)
          printParams(params)
          printDepthln()
          printPart(body, depth)
          println("\nend function\n")
        case Param(name) => print(name)
        case Literal(value) => print(value)
          // TODO Comments
        case _ =>
      }
      def printDepthln(i: Int = 0) {
        println()
        for (i <- 0 to depth + i) {
          print("    ")
        }
      }
      def printParams(params: Seq[ASTNode]) {
        print("(")
        var fst = true
        for (par <- params) {
          if (!fst) {
            print(", ")
          }
          fst = false
          printPart(par, depth)
        }
        print(")")
      }
    }
    program.functions.values.foreach(printPart(_, 0))
  }

}

object PrettyPrinter {

  def main(args: Array[String]) {
    import scala.util.control.Exception._

    val file = new File(args.headOption.getOrElse("doc/examples/test.eca"))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    val prettyPrinter = new PrettyPrinter(program.getOrElse(sys.exit(1)), errorHandler)
    prettyPrinter.printProg()
    println("Done")
  }

}
