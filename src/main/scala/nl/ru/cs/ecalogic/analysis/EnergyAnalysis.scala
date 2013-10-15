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
package analysis

import ast._
import parser.Parser
import util.{ErrorHandler, DefaultErrorHandler}

import scala.collection.mutable
import scala.io.Source

import java.io.File

import model._
// TODO: parametrize these
import model.examples.StubComponent
import model.examples.DemoComponents.CPU

/**
 * @author Marc Schoolderman
 */
class EnergyAnalysis(program: Program, eh: ErrorHandler = new DefaultErrorHandler()) {

  /** bla
    */
  def foo() {

    object T {
      val ite = CPU.T("ite")
      val a = CPU.T("a")
      val e = CPU.T("e")
      val w = CPU.T("w")
    }

    for(fundef <- program.definitions) {

      val C = StubComponent
      val G = C.EACState(C.initialState, 0, 0)
      // is t equal to the CPU's timestamp (answer: no, but can't we hack it so that it is?)
      val t: ECAValue = 0

      // NOTE: do not edit, working on this

      /** bla
       *
       * @param live Set of variable names that have been assigned
       * @param node AST node under consideration
       * @return updated set of live variables
       *
      */
      // TODO: incorporate cpu (right now we only keep track of time)
      def duracellBunny(G: (C.EACState,ECAValue), node: ASTNode): (C.EACState,ECAValue) = node match {
	case Skip()                       => G
	case If(pred, thenPart, elsePart) => val NG = duracellBunny(G,pred) match { case (g,t) => (g,t+T.ite) }
					     val (g1,t1) = duracellBunny(NG,thenPart)
					     val (g2,t2) = duracellBunny(NG,elsePart)
					     (C.lub(g1, g2), t1 max t2)
	case While(pred, rf, consq)       => throw new ECAException("while not implemented do nothing")
	case Composition(stms)            => stms.foldLeft(G)(duracellBunny); G
	case Assignment(_, expr)          => duracellBunny(G,expr) match { case (g,t) => (g,t+T.a) }
	//TODO fix eac update, because it is broken
	case FunCall(fun, args)
          if fun.isPrefixed               => args.foldLeft(G)(duracellBunny) match { case (g,t) => g.update(fun.name,t) }
	// TODO: memoize
	case FunCall(fun, args)           => G // duracellBunny(args.foldLeft(G)(duracellBunny), -->fun.body <--)
	case e:NAryExpression             => e.operands.foldLeft(G)(duracellBunny) match { case (g,t) => (g,t+T.e) }
	case _:PrimaryExpression          => G
      }

    }

  }
}

object EnergyAnalysis {

  def main(args: Array[String]) {
    import scala.util.control.Exception._

    val file = new File(args.headOption.getOrElse(sys.error("Missing argument.")))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    val checker = new SemanticAnalysis(program.getOrElse(sys.exit(1)), errorHandler)
    checker.functionCallHygiene()
    checker.variableReferenceHygiene()
    println("Done")
  }

}
