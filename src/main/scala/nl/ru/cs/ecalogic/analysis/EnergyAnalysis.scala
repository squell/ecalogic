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
import config.Options.{Analysis => config}

import scala.collection.mutable
import scala.io.Source

import java.io.File
import java.io.FileNotFoundException

import model._
// TODO: parametrize these
import model.examples._
import model.examples.DemoComponents._

/**
 * @author Marc Schoolderman
 */
class EnergyAnalysis(program: Program, components: Set[ComponentModel], eh: ErrorHandler = new DefaultErrorHandler()) {

  import GlobalState.States
  import Transform._
  import EnergyAnalysis._

  type Environment = Map[String, Expression]

  val lookup: Map[String, FunDef] =
    program.definitions.map(fundef=>fundef.name->fundef).toMap

  /** Performs the functions of both "r()" and "e()" in the FOPARA Paper
    *
    * @param t The new timestamp for components (should be in the past)
    * @param out Component states representing the cost of evaluating the loop-body "many times"
    * @param pre Component states before evaluating the loop-body
    * @return A new set of component states with updated time and energy information
    */

  def computeEnergyBound(out: GlobalState, in: GlobalState, rf: ECAValue): GlobalState = (
    out.gamma.transform((comp,g) => g.update(g.t min in.t, in(comp).e + (out(comp).e-in(comp).e)*rf)),
    in.t + (out.t-in.t)*rf
  )

  /** Performs the functions of both "r()" and "e()" in the *Technical Report*
    *
    * @param t The new timestamp for components (should be in the past)
    * @param out Component states representing the cost of evaluating the loop-body after having done it "many times"
    * @param in  Component states representing the cost of evaluating the loop-body for the first time
    * @param pre Component states before evaluating the loop-body
    * @return A new set of component states with updated time and energy information
    */

  def computeEnergyBound_TR(out: GlobalState, in: GlobalState, pre: GlobalState, rf: ECAValue): GlobalState = (
    out.gamma.transform((comp,g) => g.update(g.t min pre.t, (in(comp).e-pre(comp).e) + (out(comp).e-in(comp).e)*(rf-1) + pre(comp).e)),
    pre.t + (in.t-pre.t)*rf
  )

  /** Performs energy analysis of the function 'program'
    *
    * @return I'll tell you later, once I know. TODO
    */
  def apply(entryPoint: String = "program") = {
    /** Compute fixed points of componentstates within a while-loop
     *
     * @param init set-of-componentstates (without global time)
     * @param expr controlling condition of while-loop
     * @param stm  main body of while loop
     * @param env  current environment mapping variables to expressions (pass-thru variable)
     */
    def fixPoint(init: States, expr: Expression, stm: Statement)(implicit env: Environment): States = {
      /** Throw away the temporal information (time, energy usage) and replace it with
          that of the initial state */
      def nontemporal(st: States) = st.mapValues(_.reset)

      /** The function we are going to iterate */
      def f(st: States) =
        nontemporal(analyse(analyse((st,ECAValue.Zero), expr), stm).gamma)

      /** Starting values for the iteration */
      var cur   = nontemporal(init)
      var lub   = cur

      /** Find the fixpoint */
      val seen  = mutable.Set(cur)
      var limit = config.fixPatience
      do {
        if({limit-=1; limit} <= 0)
          eh.fatalError(new ECAException("Model error: not all component delta functions have fixed points."))
        cur  = f(cur)
        lub  = lub.transform((name,st)=>st.lub(cur(name)))
      } while(seen.add(cur))

      /** Restore the original time and energy info */
      return lub.transform((name,st)=>st.update(init(name).t, init(name).e))
    }

    /** Convert an Expression to ECAValue, and complain if this is not possible */
    def resolve(expr: Expression): ECAValue = expr match {
      case Literal(x) => x
      case _ => eh.error(new ECAException("Could not resolve this value.", expr.position)); 0
    }

    /** Energy consumption analysis
     *
     * @param G       tuple of set-of-componentstates and the global timestamp
     * @param node    AST node under consideration
     * @param env  current environment mapping variables to expressions (pass-thru variable)
     * @return        updated tuple of set-of-componentstates and global timestamp
     *
     */
    def analyse(G: GlobalState, node: ASTNode)(implicit env: Environment): GlobalState = node match {
      case FunDef(name, parms, body)    => analyse(G,body)
      case Skip()                       => G
      case If(pred, thenPart, elsePart) => val Gpre = if (config.beforeSync) G.sync else G
                                           val G2 = analyse(Gpre,pred).update("CPU","ite")
                                           val G3 = analyse(G2,thenPart)
                                           val G4 = analyse(G2,elsePart)
                                           if(config.afterSync)
                                             (G3.sync max G4.sync).timeshift
                                           else
                                             G3 max G4

      case While(pred, rf, consq)
        if config.techReport            => val Gpre = if (config.beforeSync) G.sync else G
                                           val G2 = analyse(Gpre,pred).update("CPU","w")
                                           val G3 = analyse(G2,consq)
                                           val G3fix = (fixPoint(G3.gamma, pred, consq), G3.t)
                                           val G4 = analyse(analyse(G3fix, pred).update("CPU","w"), consq)
                                         //val G4 = analyse(analyse(G3fix, pred), consq) // this is bug-compatible with the TR
                                           val iters = resolve(foldConstants(rf, env))
                                           if(config.afterSync)
                                             computeEnergyBound_TR(G4.sync, G3.sync, Gpre, iters).timeshift
                                           else
                                             computeEnergyBound_TR(G4, G3, Gpre, iters)

      case While(pred, rf, consq)       => val Gpre = if (config.beforeSync) G.sync else G
                                           val Gfix = (fixPoint(Gpre.gamma, pred, consq), Gpre.t)
                                           val G2 = analyse(Gfix,pred).update("CPU","w")
                                           val G3 = analyse(G2,consq)
                                           val iters = resolve(foldConstants(rf, env))
                                           if(config.afterSync)
                                             computeEnergyBound(G3.sync, Gpre, iters).timeshift
                                           else
                                             computeEnergyBound(G3, Gpre, iters)

      case Composition(stms)            => stms.foldLeft(G)(analyse)
      case Assignment(_, expr)          => analyse(G,expr).update("CPU","a")

      case FunCall(fun, args)
        if fun.isPrefixed               => val component = fun.prefix.get
                                           if(!G.gamma.contains(component)) {
                                             eh.error(new ECAException(s"Component not found: $component", node.position)); G
                                           } else
                                             args.foldLeft(G)(analyse).update(component, fun.name)

      case FunCall(fun, args)           => val funDef = lookup(fun.name)
                                           val resolvedArgs = args.map(foldConstants(_, env))
                                           val binding = funDef.parameters.map(_.name) zip resolvedArgs
                                           analyse(G, funDef.body)(env ++ binding)

      case e:NAryExpression             => e.operands.foldLeft(G)(analyse).update("CPU", "e")
      case _:PrimaryExpression          => G
    }

    val finalState = analyse(GlobalState.initial(Seq(HyperPentium)++components), lookup(entryPoint))(Map.empty).sync
    if(eh.errorOccurred)
      throw new ECAException("Analysis failed.")
    else
      finalState.mapValues(_.e)
  }
}

object EnergyAnalysis {

  /** For debugging (and exposition) purposes, a CPU which computes everything
    * instantly and consumes no power. (You know you want one!)
    */

  object HyperPentium extends DSLModel("CPU") {
    define T (e = 0, a = 0, w = 0, ite = 0)
    define E (e = 0, a = 0, w = 0, ite = 0)
  }

  def main(args: Array[String]) {
    import scala.util.control.Exception._
    import nl.ru.cs.ecalogic.config
    val fileName = config.Options(args).headOption.getOrElse("program.eca")
    try {
      val file = new File(fileName)
      val source = Source.fromFile(file).mkString
      val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
      val parser = new Parser(source, errorHandler)
      val program = parser.program()
      errorHandler.successOrElse("Parse errors encountered.")

      val checker = new SemanticAnalysis(program, errorHandler)
      checker.functionCallHygiene()
      checker.variableReferenceHygiene()
      errorHandler.successOrElse("Semantic errors; please fix these.")

      val components = Set(StubComponent, BadComponent, Sensor, Radio, if(config.Options.noCPU) StubComponent else CPU)

      val consumptionAnalyser = new EnergyAnalysis(program, components, errorHandler)
      println(consumptionAnalyser().toString)

      println("Success.")
    } catch {
      case e: ECAException => println(s"${e.message}\nAborted")
      case e: FileNotFoundException => println(s"File not found: ${fileName}")
    }
  }

}

