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
import interpreter.BaseInterpreter
import parser.ModelParser
import util._

import scala.collection.mutable
import scala.io.Source
import scala.util.Try

import java.io.File
import java.lang.reflect.Method
import java.net.{URI, URL}

class ECMModel(val node: Component, protected val errorHandler: ErrorHandler = new DefaultErrorHandler()) extends ComponentModel with BaseInterpreter {
  import ECAException._

  class CState private[ECMModel](val elements: Map[String, ECAValue]) extends ComponentState {

    protected def update(newElements: Map[String, ECAValue]) = new CState(newElements)

  }

  protected case class IState(locals: Map[String, ECAValue], component: Map[String, ECAValue], mutation: Boolean) extends BaseInterpreterState {

    def value(name: String) = component.get(name) orElse locals.get(name)

    def substitute(name: String, value: ECAValue, stackTrace: StackTrace) = {
      if (node.variables.contains(name)) {
        if (!mutation) {
          errorHandler.fatalError(new ECAException(s"Mutation of state variable '$name' is not allowed in this context.", stackTrace))
        }
        checkBoundaries(node.variables(name), value, Right(stackTrace))

        IState(locals, component.updated(name, value), mutation)
      } else
        IState(locals.updated(name, value), component, mutation)
    }

    def enterFunction(name: String, arguments: Map[String, ECAValue])(block: IState => IState) = {
      val postFunState = block(IState(arguments, component, mutation))
      (IState(locals, postFunState.component, mutation), postFunState.locals.get(name))
    }

  }

  node.variables.values.foreach { v =>
    checkBoundaries(v, v.initialValue.getOrElse(v.upper), Left(v.initializer.map(_.value).getOrElse(v).position))
  }

  node.functions.get("phi").filterNot(_.arity == 0).foreach { f =>
    errorHandler.fatalError(new ECAException(s"Phi function should have no parameters.", f))
  }

  val name = node.name
  val initialState = new CState(node.variables.map { case (k, v) => k -> v.initialValue.getOrElse(v.upper) })

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

  protected override val componentName = Some(name)
  protected val functions = node.functions

  private def callReflective(call: FunCall, arguments: Seq[ECAValue], stackTrace: StackTraceBuilder): ECAValue = {
    val classAlias = call.name.prefix.get
    val methodName = call.name.name
    val clazz = imports.getOrElse(classAlias, errorHandler.fatalError(new ECAException(s"Undeclared class: '$classAlias'.", stackTrace.result(call))))

    val (method, converter) = methodCache.getOrElseUpdate(call.name, {
      Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[ECAValue]):_*), (v: ECAValue) => v))                       orElse
      Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[BigInt]):_*)  , (v: ECAValue) => v.toBigInt))              orElse
      //Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[Long]):_*)    , (v: ECAValue) => Long.box(v.toLong)))      orElse
      Try((clazz.getMethod(methodName, Seq.fill(arguments.length)(classOf[Int]):_*)     , (v: ECAValue) => Int.box(v.toInt)))        getOrElse {
        errorHandler.fatalError(new ECAException(s"Method '$name' could not be found.", stackTrace.result(call)))
      }
    })

    val newStackTrace = stackTrace.callFunction(new FunName(methodName, Some(s"<external:${clazz.getName}>")), Some(call.position))
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

  override protected def evalExpression(expr: Expression, state: IState, stackTrace: StackTraceBuilder): (IState, ECAValue) = expr match {
    case call @ FunCall(qname, args) if qname.isPrefixed =>
      val (postArgsState, values) = evalExprList(args, state, stackTrace)
      (postArgsState, callReflective(call, values, stackTrace))
    case _ => super.evalExpression(expr, state, stackTrace)
  }

  override def E(f: String) = node.componentFunctions.get(f).map(_.energy).getOrElse(ECAValue.Zero)

  override def T(f: String) = node.componentFunctions.get(f).map(_.time).getOrElse(ECAValue.Zero)

  override def delta(f: String)(s: CState) = node.componentFunctions.get(f).map { f =>
    new CState(evalFunction(f, Seq.fill(f.arity)(ECAValue.Zero), IState(Map.empty, s.elements, true),
      newStackTraceBuilder(new FunName("delta", Some("<internal>"))), None)._1.component)
  }.getOrElse(s)

  override def eval(f: String)(s: CState, a: Seq[ECAValue]) = node.componentFunctions.get(f).map { f =>
    val (stateEnv, result) = evalFunction(f, a, IState(Map.empty, s.elements, true),
      newStackTraceBuilder(new FunName("eval", Some("<internal>"))), None)
    (new CState(stateEnv.component), result)
  }.getOrElse((s, ECAValue.Zero))

  override def phi(s: CState) = node.functions.get("phi").map { f =>
    evalFunction(f, Seq.empty, IState(Map.empty, s.elements, false), newStackTraceBuilder(new FunName("phi", Some("<internal>"))), None)._2
  }.getOrElse(super.phi(s))

  override def functionArity(f: String) = node.componentFunctions.get(f).map(_.arity)

  override def hasFunctionInfo = true

}

object ECMModel {

  def fromSource(sourceText: String, sourceURI: Option[URI] = None, errorHandler: Option[ErrorHandler] = None) = {
    val eh = errorHandler.getOrElse(new DefaultErrorHandler(sourceText = Some(sourceText), sourceURI = sourceURI))

    val parser = new ModelParser(sourceText, eh)
    val node = parser.component()
    parser.expectEndOfFile()
    eh.successOrElse(s"Parsing${sourceURI.fold(" ")(u => s" '$u' ")}'failed.")

    sourceURI.foreach { uri =>
      val path = uri.getPath
      val fileName = path.substring(path.lastIndexOf('/') + 1, path.length)
      if (fileName != node.name + ".ecm") {
        eh.warning(new ECAException(s"File name does not match component name '${node.name}'.", node))
      }
    }

    eh.reset()
    new ECMModel(node, eh)
  }

  def fromFile(file: File, errorHandler: Option[ErrorHandler] = None): ECMModel = {
    val source = Source.fromFile(file).mkString
    fromSource(source, Some(file.toURI), errorHandler)
  }

  def fromURL(url: URL, errorHandler: Option[ErrorHandler] = None): ECMModel = {
    val source = Source.fromURL(url).mkString
    fromSource(source, Some(url.toURI), errorHandler)
  }

}
