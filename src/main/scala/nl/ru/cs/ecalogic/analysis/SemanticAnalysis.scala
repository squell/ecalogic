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

import nl.ru.cs.ecalogic.util.{ErrorHandler, DefaultErrorHandler}
import nl.ru.cs.ecalogic.ast._

import nl.ru.cs.ecalogic.parser.Parser
import nl.ru.cs.ecalogic.ECAException
import scala.util.control.Exception._
import scala.io.Source
import java.io.File


/**
 * @author Marc Schoolderman
 */
class SemanticAnalysis(program: Program, eh: ErrorHandler = new DefaultErrorHandler()) {

  /** Sanity checks function calls in ECA programs; specifically:
    * Number of argument should match number of parameters, no recursion
    */
  def functionCallHygiene() {
    val defs = program.definitions;

    for(i <- 0 to defs.length-1) 
      if(defs.view(0,i).contains(defs(i)))
        eh.error(new ECAException("Redefinition of function "+defs(i).name, defs(i).position))

    val arity: Map[String, Int] =
      defs.map(fundef=>fundef.name->fundef.parameters.length).toMap

    def funCalls(node: ASTNode): Set[String] = node match {
      case If(pred, thenPart, elsePart) => funCalls(pred) ++ funCalls(thenPart) ++ funCalls(elsePart)
      case While(pred, _, consq)        => funCalls(pred) ++ funCalls(consq)
      case Composition(stms)            => stms.flatMap(funCalls).toSet
      case Assignment(_, expr)          => funCalls(expr)
      case e: BinaryExpression          => funCalls(e.left) ++ funCalls(e.right)
      case e: UnaryExpression           => funCalls(e.operand)
      case FunCall(fun, args)
        if !fun.isPrefixed              => arity.getOrElse(fun.name, -1) match {
                                             case -1                    => eh.error(new ECAException("Undefined function: "+fun.name, node.position))
                                             case x if x != args.length => eh.error(new ECAException("Incorrect number of arguments", node.position))
					     case _                     => 
                                           }
                                           args.flatMap(funCalls).toSet + fun.name
      case _                            => Set.empty
    }

    val calls: Map[String, Set[String]] =
      defs.map(fundef=>fundef.name->funCalls(fundef.body)).toMap.withDefaultValue(Set.empty)

    def detectCycle(seen: Set[String], open: Set[String]) {
      for(next <- open) 
        if(seen(next))
          eh.error(new ECAException("Recursion in function "+next+" is not allowed"))
        else
          detectCycle(seen+next, calls(next))
    }
    detectCycle(Set.empty, calls.keys.toSet)
  }

  /** Checks whether variables could be used uninitialized
    */
  def uninitializedVars() {

    def varFlow(live: Set[String], node: ASTNode): Set[String] = {
      node match {
        case If(pred, thenPart, elsePart) => varFlow(live, pred); 
                                             return varFlow(live, thenPart) & varFlow(live, elsePart)
        case While(pred, rf, consq)       => varFlow(live, pred); varFlow(live, rf); varFlow(live, consq)
        case Composition(stms)            => stms.foldLeft(live)(varFlow)
        case Assignment(ident, expr)      => varFlow(live, expr); 
                                             return live + ident
        case e: BinaryExpression          => varFlow(live, e.left); varFlow(live, e.right)
        case e: UnaryExpression           => varFlow(live, e.operand)
        case FunCall(fun, args)
          if !fun.isPrefixed              => args.map(varFlow(live,_))
        case VarRef(ident)                => if(!live(ident))
                                               eh.warning(new ECAException("Variable "+ident+" may be used uninitialized", node.position))
        case _                            => ;
      }
      return live;
    }

    for(fundef <- program.definitions)
      varFlow(fundef.parameters.map(_.name).toSet, fundef.body)
  }
}

object SemanticAnalysis {
  def main(args: Array[String]) {
    val file = new File(args.headOption.getOrElse("zooi/test.eca"))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    val checker = new SemanticAnalysis(program.getOrElse(sys.exit(1)), errorHandler)
    checker.functionCallHygiene
    checker.uninitializedVars
    println("Done")
  }
}
