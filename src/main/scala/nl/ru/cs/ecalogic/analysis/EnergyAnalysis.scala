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

      def regress(oldState: (States,ECAValue)) =
         c.mapValues(_.regress(oldState._2)) -> t

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

    /** TODO It is unclear from the tech report what the resulting *state* should be 
        it is probably 'out'. But not sure. I don't trust the specification on this part yet.
      */
    def e(out: States, in: States, pre: States, rf: ECAValue) = {
      out.transform((comp,g) => g.setEnergy((in(comp).e-pre(comp).e) + (out(comp).e-in(comp).e)*(rf-1)))
    }

    def fixPoint(gamma: (States,ECAValue), expr: Expression, stm: Statement) = {
      //TODO
      def fix(st: ComponentModel#EACState, expr: Expression, stm: Statement) = {
        // "the concatenation of all df applied in S" -- this is too ambiguous to implement
        // what if a single df is applied multiple times, do we apply it multiple times?
        // what if one df is used in the 'then' part and the other in 'else', do we apply both?
        st
      }
      gamma._1.transform((comp,g) => fix(g, expr, stm)) -> gamma._2
    }

    for(fundef <- program.definitions) {

      /** Energy consumption analysis
       *
       * @param G       tuple of set-of-componentstates and the global timestamp
       * @param node    AST node under consideration
       * @return        updated tuple of set-of-componentstates and global timestamp
       *
      */
      def duracellBunny(G: (States,ECAValue), node: ASTNode): (States,ECAValue) = node match {
        case Skip()                       => G
        case If(pred, thenPart, elsePart) => val G2 = duracellBunny(G,pred).update("CPU","ite")
                                             val G3 = duracellBunny(G2,thenPart) 
                                             val G4 = duracellBunny(G2,elsePart)
                                             G3 max G4

        case While(pred, rf, consq)       => val G2 = duracellBunny(G,pred).update("CPU","w")
                                             // this next match will be removed in later versions
                                             val iters = rf match { case Literal(x) => x }
                                             // (dont care about nice error msgs at this point)
                                             val G3 = duracellBunny(G2,consq)
                                             val G3fix = fixPoint(G3, pred, consq)
                                             val G4 = duracellBunny(duracellBunny(G3fix, pred), consq)
                                             // I'm not sure i understand this, but this is what the paper says.
                                             e(G4.regress(G)._1, G3._1, G._1, iters) -> (G3._2-G._2)*iters

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
