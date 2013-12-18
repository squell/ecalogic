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
package model

import ast._
import parser.ModelParser
import util._

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

import java.io.File
import java.lang.reflect.Method
import java.net.{URI, URL}

class ECMModel(node: Component, errorHandler: ErrorHandler = new DefaultErrorHandler()) extends ComponentModel {
  import ECAException._

  class CState private[ECMModel](val elements: Map[String, ECAValue]) extends ComponentState {

    protected def update(newElements: Map[String, ECAValue]) = new CState(newElements)

  }

  node.variables.values.foreach { v =>
    checkBoundaries(v, v.initialValue.getOrElse(ECAValue.Zero), Left(v.initializer.map(_.value).getOrElse(v).position))
  }

  node.functions.get("phi").filterNot(_.arity == 0).foreach { f =>
    errorHandler.fatalError(new ECAException(s"Phi function should have no parameters.", f))
  }

  val name = node.name
  val initialState = new CState(node.variables.mapValues(_.initialValue.getOrElse(ECAValue.Zero)))

  private val methodCache = mutable.Map.empty[FunName, (Method, ECAValue => AnyRef)]
  private val imports     = node.imports.mapValues { i =>
    try {
      Class.forName(i.qualifiedName)
    } catch {
      case e: Exception => errorHandler.fatalError(new ECAException(s"Error loading imported class '${i.qualifiedName}': $e", i))
    }
  }

  private def checkBoundaries(variable: CompVarDecl, value: ECAValue, info: Either[Position, StackTrace]) {
    if (value < variable.lower || value > variable.upper) {
      errorHandler.fatalError(new ECAException(s"Value $value exceeds the specified boundaries: ${variable.lower} <= ${variable.name} <= ${variable.upper}.",
        info.left.toOption, None, info.right.getOrElse(Seq.empty)))
    }
  }

  private def evalFunction(fun: BasicFunction, arguments: Seq[ECAValue], state: Map[String, ECAValue],
                           stackTrace: StackTraceBuilder, callPosition: Option[Position]): (Map[String, ECAValue], ECAValue) = {
    if (arguments.length != fun.arity) {
      errorHandler.fatalError(new ECAException(s"Function '${fun.name}' requires ${fun.arity} arguments; given: ${arguments.length}.", stackTrace.result(callPosition)))
    }
    val (localEnv, stateEnv) = evalStatement(fun.body, fun.parameters.map(_.name).zip(arguments).toMap,
      state, fun.isComponent, stackTrace.callFunction(new FunName(fun.name, if (fun.isComponent) Some(name) else None), callPosition))
    (stateEnv, localEnv.getOrElse(fun.name, ECAValue.Zero))
  }

  private def evalFunction(funName: String, arguments: Seq[ECAValue], state: Map[String, ECAValue],
                           stackTrace: StackTraceBuilder, callPosition: Option[Position]): ECAValue =
    node.functions.get(funName).map(evalFunction(_, arguments, state, stackTrace, callPosition)).getOrElse {
      errorHandler.fatalError(new ECAException(s"Undeclared function: '$funName'.", stackTrace.result(callPosition)))
    }._2

  private def evalStatement(stmt: Statement, localEnv: Map[String, ECAValue], stateEnv: Map[String, ECAValue],
                            mutationAllowed: Boolean, stackTrace: StackTraceBuilder): (Map[String, ECAValue], Map[String, ECAValue]) = {
    def evalStmt(stmt: Statement): (Map[String, ECAValue], Map[String, ECAValue]) = stmt match {
      case Skip() =>
        (localEnv, stateEnv)
      case Assignment(variable, expr) if node.variables.contains(variable) =>
        if (!mutationAllowed) {
          errorHandler.fatalError(new ECAException(s"Mutation of state variable '$variable' is not allowed in this context.", stackTrace.result(stmt)))
        }
        val value = evalExpression(expr, localEnv, stateEnv, stackTrace)
        checkBoundaries(node.variables(variable), value, Right(stackTrace.result(expr)))
        (localEnv, stateEnv.updated(variable, value))
      case Assignment(variable, expr) =>
        (localEnv.updated(variable, evalExpression(expr, localEnv, stateEnv, stackTrace)), stateEnv)
      case f: FunCall =>
        evalExpression(f, localEnv, stateEnv, stackTrace) // throw away the result
        (localEnv, stateEnv)
      case If(predicate, consequent, alternative) if evalExpression(predicate, localEnv, stateEnv, stackTrace) =>
        evalStmt(consequent)
      case If(predicate, consequent, alternative) =>
        evalStmt(alternative)
      case While(predicate, _, consequent) if evalExpression(predicate, localEnv, stateEnv, stackTrace) =>
        evalStmt(Composition(Seq(consequent, stmt)))
      case While(_, _, _) =>
        (localEnv, stateEnv)
      case Composition(statements) =>
        statements.foldLeft((localEnv, stateEnv)) {
          case ((localEnv, stateEnv), stmt) => evalStatement(stmt, localEnv, stateEnv, mutationAllowed, stackTrace)
        }
    }

    evalStmt(stmt)
  }

  private def evalExpression(expr: Expression, localEnv: Map[String, ECAValue], stateEnv: Map[String, ECAValue], stackTrace: StackTraceBuilder): ECAValue = {
    def callReflective(name: FunName, arguments: Seq[ECAValue], stackTrace: StackTraceBuilder, callPosition: Option[Position]): ECAValue = {
      val classAlias = name.prefix.get
      val methodName = name.name
      val clazz = imports.getOrElse(classAlias, errorHandler.fatalError(new ECAException(s"Undeclared class: '$classAlias'.", stackTrace.result(expr))))

      val (method, converter) = methodCache.getOrElseUpdate(name, {
        Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[ECAValue]):_*), (v: ECAValue) => v))                       orElse
        //Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[BigInteger]):_*), (v: ECAValue) => v.toBigInt.underlying())) orElse
        Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[BigInt]):_*)  , (v: ECAValue) => v.toBigInt))              orElse
        //Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[Long]):_*)    , (v: ECAValue) => Long.box(v.toLong)))      orElse
        Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[Int]):_*)     , (v: ECAValue) => Int.box(v.toInt)))        getOrElse {
          errorHandler.fatalError(new ECAException(s"Method '$name' could not be found.", stackTrace.result(expr)))
        }
      })

      val newStackTrace = stackTrace.callFunction(new FunName(methodName, Some(s"<external:${clazz.getName}>")), callPosition)
      val result = try {
        method.invoke(null, arguments.map(converter):_*)
      } catch {
        case e: Exception =>
          val trace = newStackTrace.result()
          errorHandler.fatalError(new ECAException(s"Exception while calling '$name': $e", trace.headOption.flatMap(_._2), Some(e), trace))
      }

      result match {
        case v: ECAValue => v
        case v: BigInt   => ECAValue.bigIntToValue(v)
        //case v: Long     => ECAValue.bigIntToValue(v)
        case v: Integer  => ECAValue.intToValue(v)
        case v           =>
          errorHandler.fatalError(new ECAException(s"Method '$name' returned an unsupported result of type: '${v.getClass.getSimpleName}'.", newStackTrace.result()))
      }
    }

    def evalExpr(expr: Expression): ECAValue = expr match {
      case Literal(value)       => value
      case VarRef(name)         => stateEnv.getOrElse(name, localEnv.getOrElse(name, errorHandler.fatalError(new ECAException(s"Undeclared variable: '$name'", stackTrace.result(expr)))))

      case And(x, y)            => evalExpr(x) && evalExpr(y)
      case Or(x, y)             => evalExpr(x) || evalExpr(y)

      case Add(x, y)            => evalExpr(x) + evalExpr(y)
      case Subtract(x, y)       => evalExpr(x) - evalExpr(y)
      case Multiply(x, y)       => evalExpr(x) * evalExpr(y)
      case Divide(x, y)         =>
        val valueY = evalExpr(y)
        if (valueY == ECAValue.Zero) {
          errorHandler.fatalError(new ECAException(s"Division by zero.", stackTrace.result(y)))
        }
        evalExpr(x) / valueY
      case Exponent(x, y)       => evalExpr(x) ^ evalExpr(y)

      case EQ(x, y)             => evalExpr(x) == evalExpr(y)
      case NE(x, y)             => evalExpr(x) != evalExpr(y)
      case LT(x, y)             => evalExpr(x) < evalExpr(y)
      case LE(x, y)             => evalExpr(x) <= evalExpr(y)
      case GT(x, y)             => evalExpr(x) > evalExpr(y)
      case GE(x, y)             => evalExpr(x) >= evalExpr(y)

      case FunCall(qname, args) if qname.isPrefixed => callReflective(qname, args.map(evalExpr), stackTrace, Some(expr.position))
      case FunCall(qname, args) => evalFunction(qname.name, args.map(evalExpr), stateEnv, stackTrace, Some(expr.position))
    }

    evalExpr(expr)
  }

  override def E(f: String) = node.componentFunctions.get(f).map(_.energy).getOrElse(ECAValue.Zero)

  override def T(f: String) = node.componentFunctions.get(f).map(_.time).getOrElse(ECAValue.Zero)

  override def delta(f: String)(s: CState) = node.componentFunctions.get(f).map { f =>
    new CState(evalFunction(f, Seq.fill(f.arity)(ECAValue.Zero), s.elements, newStackTraceBuilder(new FunName("delta", Some("<internal>"))), None)._1)
  }.getOrElse(s)

  override def eval(f: String)(s: CState, a: Seq[ECAValue]) = node.componentFunctions.get(f).map { f =>
    val (stateEnv, result) = evalFunction(f, a, s.elements, newStackTraceBuilder(new FunName("eval", Some("<internal>"))), None)
    (new CState(stateEnv), result)
  }.getOrElse((s, ECAValue.Zero))

  override def phi(s: CState) = node.functions.get("phi").map { f =>
    evalFunction(f, Seq.empty, s.elements, newStackTraceBuilder(new FunName("phi", Some("<internal>"))), None)._2
  }.getOrElse(super.phi(s))

  override def functionArity(f: String) = node.componentFunctions.get(f).map(_.arity)

  override def hasFunctionInfo = true

}

object ECMModel {

  // FIXME TODO: doesn't creating error handlers inside these functions defeats the purpose of the errorhandler?
  def fromSource(sourceText: String, sourceURI: Option[URI] = None, eh: ErrorHandler = null) = {
    val errorHandler = if(eh == null) new DefaultErrorHandler(sourceText = Some(sourceText), sourceURI = sourceURI) else eh

    val parser = new ModelParser(sourceText, errorHandler)
    val node = parser.component()
    parser.expectEndOfFile()
    errorHandler.successOrElse(s"Parsing${sourceURI.fold(" ")(u => s" '$u' ")}'failed.")

    sourceURI.foreach { uri =>
      val path = uri.getPath
      val fileName = path.substring(path.lastIndexOf('/') + 1, path.length)
      if (fileName != node.name + ".ecm") {
        errorHandler.fatalError(new ECAException(s"File name does not match component name '${node.name}'."))
      }
    }

    errorHandler.reset()
    new ECMModel(node, errorHandler)
  }

  def fromFile(file: String): ECMModel = fromFile(new File(file))

  def fromFile(file: File): ECMModel = {
    val source = Source.fromFile(file).mkString
    fromSource(source, Some(file.toURI))
  }

  def fromURL(url: String): ECMModel = fromURL(new URL(url))

  def fromURL(url: URL): ECMModel = {
    val source = Source.fromURL(url).mkString
    fromSource(source, Some(url.toURI))
  }

}
