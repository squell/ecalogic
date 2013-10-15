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

    /** Hardcoded for now */
    val Components = Set(CPU, StubComponent)

    /** A map containing, for each component (identified by name), its current component state */
    type States = Map[String,ComponentModel#EACState]

    /** Using the "Pimp My Library" pattern to define two useful functions on the global state */
    implicit class StateMutator(G: (States,ECAValue)) {
      // why no pattern match on^ this baby?
      final val (c, t) = G
      def update(comp: String, fun: String) = {
	val (g, t1) = c(comp).update(fun, t)
	c.updated(comp, g) -> t1
      }

      // there is a cute problem with this function:
      // Scala doesn't know that the two EACStates are
      // the same type.
      def max(that: (States,ECAValue)) = {
	val (c2, t2) = that
        c.transform((comp,g)=> g.lub(c2(comp))) -> t.max(t2)
      }
    }

    val lookup: Map[String, FunDef] =
      program.definitions.map(fundef=>fundef.name->fundef).toMap

    for(fundef <- program.definitions) {

      /** Energy consumption analysis
       *
       * @param G	tuple of set-of-componentstates and the global timestamp
       * @param node	AST node under consideration
       * @return 	updated tuple of set-of-componentstates and global timestamp
       *
      */
      def duracellBunny(G: (States,ECAValue), node: ASTNode): (States,ECAValue) = node match {
	case Skip()                       => G
	case If(pred, thenPart, elsePart) => val NG = duracellBunny(G,pred).update("CPU","ite")
					     val G1 = duracellBunny(NG,thenPart) 
					     val G2 = duracellBunny(NG,elsePart)
					     G1 max G2

	case While(pred, rf, consq)       => val NG = duracellBunny(G,pred).update("CPU","w")
					     // this next match will be removed in later versions
	                                     val iters = rf match { case Literal(x) => x }
					     // (dont care about nice error msgs at this point)
					     throw new ECAException("while not implemented do nothing")

	case Composition(stms)            => stms.foldLeft(G)(duracellBunny)
	case Assignment(_, expr)          => duracellBunny(G,expr).update("CPU","a")
	case FunCall(fun, args)
          if fun.isPrefixed               => args.foldLeft(G)(duracellBunny).update(fun.prefix.get, fun.name)
	case FunCall(fun, args)           => val body = lookup(fun.name).body
	                                     duracellBunny(args.foldLeft(G)(duracellBunny), body)
	case e:NAryExpression             => e.operands.foldLeft(G)(duracellBunny).update("CPU", "e")
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
