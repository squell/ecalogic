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
import java.io.FileNotFoundException

import model._
// TODO: parametrize these
import model.examples._
import model.examples.DemoComponents.CPU

/**
 * @author Marc Schoolderman
 */
class EnergyAnalysis(program: Program) {

  import GlobalState.States

  /** For debugging (and exposition) purposes, a CPU which computes everything
    * instantly and consumes no power. (You know you want one!)
    */
  object HyperPentium extends DSLModel("CPU") {
    define T (e = 0, a = 0, w = 0, ite = 0)
    define E (e = 0, a = 0, w = 0, ite = 0)
  }

  type Environment = Map[String, Expression]

  val components = Set(HyperPentium, StubComponent, BadComponent)

  val lookup: Map[String, FunDef] =
    program.definitions.map(fundef=>fundef.name->fundef).toMap

  /** Performs energy analysis of the function 'program'
    *
    * @return I'll tell you later, once I know. TODO
    */
  def apply(entryPoint: String = "program") = {
    /** Performs the functions of both "r()" and "e()" in the paper
      *
      * @param t The new timestamp for components (should be in the past)
      * @param out Component states *after* having evaluated the loop condition and loop body
      * @param in  Component states *after* having evaluated the loop condition, but before evaluating the body
      * @param pre Component states *before* evaluating the entire while loop
      * @return A new set of component states with updated time and energy information
      */
    def computeEnergyBound(t:ECAValue, out: States, in: States, pre: States, rf: ECAValue) =
      out.transform((comp,g) => g.update(t, (in(comp).e-pre(comp).e) + (out(comp).e-in(comp).e)*(rf-1) + pre(comp).e))

    /** Compute fixed points of stats
      */
    def fixPoint(init: States, expr: Expression, stm: Statement)(implicit env: Environment): States = {
      var cur  = init
      var prev = init

      /** Throw away the temporal information (time, energy usage) and replace it with
          that of the initial state */
      def nontemporal(st: States) = st.mapValues(_.reset)

      /** The function we are going to iterate */
      def f(st: States) =
        nontemporal(duracellBunny(duracellBunny(st->ECAValue.Zero, expr), stm).gamma)

      /** Even though it may not look it, this will always terminate. */
      var limit = 1000;
      do {
        if({limit-=1; limit} <= 0)
          throw new ECAException("Model error: state delta functions not monotone")
        prev = cur
        cur  = f(cur)
      } while(cur != prev)

      /** Restore the original time and energy info */
      return cur.transform((name,st)=>st.update(init(name).t, init(name).e))
    }

    /** Energy consumption analysis
     *
     * @param G       tuple of set-of-componentstates and the global timestamp
     * @param node    AST node under consideration
     * @return        updated tuple of set-of-componentstates and global timestamp
     *
     */
    def duracellBunny(G: GlobalState, node: ASTNode)(implicit env: Environment): GlobalState = node match {
      case FunDef(name, parms, body)    => duracellBunny(G,body)
      case Skip()                       => G
      case If(pred, thenPart, elsePart) => val G2 = duracellBunny(G,pred).update("CPU","ite")
                                           val G3 = duracellBunny(G2,thenPart)
                                           val G4 = duracellBunny(G2,elsePart)
                                           G3 max G4

      case While(pred, rf, consq)       => val Gpre = G.sync
                                           val G2 = duracellBunny(Gpre,pred).update("CPU","w")
                                           // this next match will be removed in later versions
                                           val iters = rf match { case Literal(x) => x }
                                           // (dont care about nice error msgs at this point)
                                           val G3 = duracellBunny(G2,consq)
                                           val G3fix = fixPoint(G3.gamma, pred, consq) -> G3.t
                                           val G4 = duracellBunny(duracellBunny(G3fix, pred), consq)
                                           // I'm not sure i understand this, but this is what the paper says.
                                           computeEnergyBound(G.t, G4.gamma, G3.gamma, Gpre.gamma, iters) -> (G.t+(G3.t-G.t)*iters)

      case Composition(stms)            => stms.foldLeft(G)(duracellBunny)
      case Assignment(_, expr)          => duracellBunny(G,expr).update("CPU","a")
      case FunCall(fun, args)
        if fun.isPrefixed               => args.foldLeft(G)(duracellBunny).update(fun.prefix.get, fun.name)
      case FunCall(fun, args)           => val body = lookup(fun.name).body
                                           duracellBunny(args.foldLeft(G)(duracellBunny), body)
      case e:NAryExpression             => e.operands.foldLeft(G)(duracellBunny).update("CPU", "e")
      case _:PrimaryExpression          => G
    }

    duracellBunny(GlobalState.initial(components), lookup(entryPoint))(Map.empty).sync.mapValues(_.e)
  }
}

object EnergyAnalysis {

  def main(args: Array[String]) {
    import scala.util.control.Exception._
    val fileName = args.headOption.getOrElse(sys.error("Missing argument."))
    try {
      val file = new File(fileName)
      val source = Source.fromFile(file).mkString
      val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
      val parser = new Parser(source, errorHandler)
      val program = parser.program()
      errorHandler.successOrElse("Parse errors encountered")

      val checker = new SemanticAnalysis(program, errorHandler)
      checker.functionCallHygiene()
      checker.variableReferenceHygiene()
      errorHandler.successOrElse("Semantic errors; please fix these.")

      val consumptionAnalyser = new EnergyAnalysis(program)
      println(consumptionAnalyser().toString)
      println("Success.")
    } catch {
      case e: ECAException => println(s"${e.message}; aborting")
      case e: FileNotFoundException => println(s"File not found: ${fileName}")
    }
  }

}
