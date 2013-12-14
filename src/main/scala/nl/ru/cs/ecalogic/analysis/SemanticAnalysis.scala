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
import model.ComponentModel
import parser.Parser
import util.{ErrorHandler, DefaultErrorHandler}

import scala.io.Source

import java.io.File

/**
 * @author Marc Schoolderman
 */
class SemanticAnalysis(program: Program, components: Map[String, ComponentModel], eh: ErrorHandler = new DefaultErrorHandler()) {

  /** Sanity checks function calls in ECA programs; specifically:
    * Number of argument should match number of parameters, no recursion
    */
  def functionCallHygiene() {
    val defs = program.functions

//    val funNames = mutable.Set.empty[String]
//    defs.foreach { f =>
//      if (funNames(f.name))
//        eh.error(new ECAException(s"Redefinition of function '${f.name}'.", f.position))
//      else
//        funNames += f.name
//    }
    val funNames = defs.keys

//    val arity: Map[String, Int] =
//      defs.map(fundef=>fundef.name->fundef.parameters.length).toMap

    def funCalls(node: ASTNode): Set[String] = node match {
      case If(pred, thenPart, elsePart) => funCalls(pred) ++ funCalls(thenPart) ++ funCalls(elsePart)
      case While(pred, _, consq)        => funCalls(pred) ++ funCalls(consq)
      case Composition(stms)            => stms.flatMap(funCalls).toSet
      case Assignment(_, expr)          => funCalls(expr)
      case FunCall(fun, args)           =>
        // FIXME: rewrite this, it doesn't really work
        if (!fun.prefix.flatMap(components.get(_).map(_.hasFunctionInfo)).getOrElse(true)) {
          eh.warning(new ECAException(s"Unchecked function call because component '${fun.prefix.get}' has no function information.", node))
        } else {
          val arity = fun.prefix.map(components.get(_).flatMap(_.functionArity(fun.name))) getOrElse defs.get(fun.name).map(_.arity)
          arity match {
           case None                  => eh.error(new ECAException(s"Undeclared function: '${fun.qualified}'.", node))
           case Some(x) if x >= 0 && x != args.length
                                      => eh.error(new ECAException(s"Incorrect number of arguments for '${fun.qualified}'. Expected: $x; found: ${args.length}.", node))
           case _                     =>
          }
        }
        args.flatMap(funCalls).toSet + fun.qualified
      case e: Expression                => e.operands.flatMap(funCalls).toSet
      case _                            => Set.empty
    }

    /* IMPORTANT: workaround for a scala 'feature'(?)
       if you use 'mapValues' here, funCalls gets to run
       twice (with side effects and all) */
    val calls: Map[String, Set[String]] =
      //defs.mapValues(f => funCalls(f.body)).toMap.withDefaultValue(Set.empty)
      defs.transform((_,f) => funCalls(f.body)).toMap.withDefaultValue(Set.empty)

    def detectCycle(seen: Set[String], open: Set[String]) {
      for(next <- open)
        if(seen(next))
          eh.error(new ECAException(s"Recursion in function '$next' is not allowed."))
        else
          detectCycle(seen+next, calls(next))
    }
    detectCycle(Set.empty, calls.keys.toSet)
  }

  /** Checks whether all variable references are proper;
      In particular, disallow programs for which it cannot be determined (statically) if references to it
      are preceeded by assignments. Also disallow values constructed at run-time from being used inside
      ranking funcions.
    */
  def variableReferenceHygiene() {

    for(fundef <- program.functions.values) {

      val params = fundef.parameters.map(_.name).toSet

      /** Determine if all variable references are potentially used uninitialized.
       *
       * @param live Set of variable names that have been assigned
       * @param node AST node under consideration
       * @return updated set of live variables
       *
      */
      def varFlow(live: Set[String], node: ASTNode): Set[String] = node match {
        case If(pred, thenPart, elsePart) => varFlow(live, pred)
                                             varFlow(live, thenPart) & varFlow(live, elsePart)
        case While(pred, Some(rf), consq) => varFlow(live, pred); varFlow(live, consq)
                                             rf match {
                                               case _: ArithmeticExpression =>
                                               case Literal(_) =>
                                               case v@VarRef(ident) =>
                                                 if(!params(ident))
                                                   eh.error(new ECAException(s"Non-parameter '$ident' not allowed in a bound expression.", v.position))
                                                 if(live(ident))
                                                   eh.warning(new ECAException(s"Variable '$ident' written to before this reference.", v.position))
                                               case e: Expression =>
                                                   eh.error(new ECAException(s"Expression not suitable for use in a ranking function.", e.position))
                                             }
                                             live
        case Composition(stms)            => stms.foldLeft(live)(varFlow)
        case Assignment(ident, expr)      => varFlow(live, expr)
                                             live + ident

        case FunCall(fun, args)
          if fun.isPrefixed               => args.foreach(varFlow(live,_))
                                             live

        // TODO: once we have "annotations", we may be more permissive as to what we can do with function calls. this following restriction severely
        // restricts the power of our language. we could also determine which parameters may be used as loop bounds and be more permissive about those.

        case FunCall(fun, args)           => args.foreach(_.foreach {
                                               case _: ArithmeticExpression =>
                                               case Literal(_) =>
                                               case VarRef(ident) if !live(ident) && params(ident) =>
                                               case e: Expression =>
                                                   eh.error(new ECAException(s"Sorry, not in this version: '$e' as argument to call.", e.position))
                                             })
                                             live
        case VarRef(ident)                => if(!live(ident) && !params(ident))
                                               eh.warning(new ECAException(s"Variable '$ident' may be used uninitialized.", node.position))
                                             live
        case e: Expression                => e.operands.foreach(varFlow(live, _)); live
        case _                            => live
      }

      varFlow(Set.empty[String], fundef.body)
    }

  }
}

object SemanticAnalysis {

  def main(args: Array[String]) {
    import scala.util.control.Exception._

    val file = new File(args.headOption.getOrElse(sys.error("Missing argument.")))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(sourceText = Some(source), sourceURI = Some(file.toURI))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    val checker = new SemanticAnalysis(program.getOrElse(sys.exit(1)), Map.empty, errorHandler)
    checker.functionCallHygiene()
    checker.variableReferenceHygiene()
    println("Done")
  }

}
