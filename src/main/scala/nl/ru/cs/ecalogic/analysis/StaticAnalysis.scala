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

package nl.ru.cs.ecalogic.analysis

import nl.ru.cs.ecalogic.util.DefaultErrorHandler
import nl.ru.cs.ecalogic.ast._

import nl.ru.cs.ecalogic.parser.Parser
import nl.ru.cs.ecalogic.ECAException
import scala.util.control.Exception._
import scala.io.Source
import java.io.File


/**
 * @author Marc Schoolderman
 */
object StaticAnalysis {

/** Detects recursion
  *
  * @param program      parsed syntax tree of a tentative eca program
  */
  def containsRecursion(program: Program) = {
    // todo: rewrite in a more imperative (less wasteful) style - this was just a test.
    def funCalls(node: ASTNode): Set[String] = node match {
      // todo: assignment
      case If(_, thenPart, elsePart) => funCalls(thenPart) ++ funCalls(elsePart)
      case While(pred, _, consq)     => funCalls(pred) ++ funCalls(consq)
      case Composition(stms)         => stms.flatMap(funCalls).toSet
      case e: BinaryExpression       => funCalls(e.left) ++ funCalls(e.right)
      case e: UnaryExpression        => funCalls(e.operand)
      case FunCall(fun, args)
        if !fun.isPrefixed           => args.flatMap(funCalls).toSet + fun.name
      case _                         => Set.empty
    }

    val calls: Map[String, Set[String]] =
      (for (fundef<-program.definitions) yield fundef.name -> funCalls(fundef.body)).toMap.withDefaultValue(Set.empty)

    def hasCycle(seen: Set[String], open: Set[String]): Boolean =
      (for (next<-open) yield seen(next) || hasCycle(seen+next, calls(next))).contains(true)

    // todo: report which call causes the recursion
    hasCycle(Set.empty, calls.keys.toSet)
  }


  def main(args: Array[String]) {
    val file = new File(args.headOption.getOrElse("zooi/test.eca"))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    println(containsRecursion(program.getOrElse(sys.exit(1))))
  }

}
