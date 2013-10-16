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

package nl.ru.cs.ecalogic.parser

import nl.ru.cs.ecalogic.ast.Program
import nl.ru.cs.ecalogic.util._
import nl.ru.cs.ecalogic.ast._
import nl.ru.cs.ecalogic.ECAException
import java.io.File
import scala.io.Source
import scala.util.control.Exception._

/**
 * @author Dorus Peelen
 */
class PretyPrinter(program: Program, eh: ErrorHandler = new DefaultErrorHandler()) {

  /** Print the prog */
  def printProg() {
    def printPart(node: ASTNode, depth: Int) {

      node match {
        case If(pred, thenPart, elsePart) =>
          print("If ");
          printPart(pred, depth)
          print(" then ");
          printPart(thenPart, depth + 1)
          print(" else ");
          printPart(elsePart, depth + 1)
          print("end if");
        case While(pred, rf, consq) =>
          print("while ")
          printPart(pred, depth)
          print(" bound ")
          printPart(rf, depth)
          println(" do")
          printDepth(1)
          printPart(consq, depth + 1);
          println
          printDepth()
          print("end while")
        case Composition(stms) =>
          var fst = true; for (stmt <- stms) {
            if (!fst) {
              println(";")
              printDepth()
            }
            fst = false;
            printPart(stmt, depth)
          }
        case Assignment(ident, expr) =>
          print(ident + " := ")
          printPart(expr, depth)
        case FunCall(fun, args) =>
          if (fun.isPrefixed) print(fun.prefix + "::") else ""
          print(fun.name);
          args.foreach(printPart(_, depth));
        case VarRef(ident) => print(ident)
        case e: BinaryExpression => 
          printPart(e.left, depth)
          print(" " + e.operator + " ")
          printPart(e.right, depth)
        case e: UnaryExpression =>
          printPart(e.operand, depth);
          e.operands.foreach(printPart(_, depth))
        case FunDef(name, param, body) =>
          print("function " + name + "(")
          var fst = true; for (par <- param) {
            if (!fst) {
              print(", ")
            }
            fst = false;
            printPart(par, depth)
          }
          println(")")
          printDepth()
          printPart(body, depth)
          println("\nend function\n")
        case Param(name) => print(name)
        case Literal(value) => print(value)
        case _ =>
      }
      def printDepth(i : Int = 0) {
        for (i <- 0 to depth+i) {
          print("    ")
        }
      }
    }
    program.definitions.foreach(printPart(_, 0))
  }
}

object PretyPrint {
  def main(args: Array[String]) {
    val file = new File(args.headOption.getOrElse("doc/examples/test.eca"))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    val pretyPrinter = new PretyPrinter(program.getOrElse(sys.exit(1)), errorHandler)
    pretyPrinter.printProg()
    println("Done")
  }
}
