package nl.ru.cs.ecalogic
package interpreter

import ast._
import model._
import parser.Parser
import analysis.SemanticAnalysis
import util.{DefaultErrorHandler, ErrorHandler}

import scala.io.Source

import java.io.File

class Interpreter(program: Program, components: Map[String, ComponentModel], protected val errorHandler: ErrorHandler = new DefaultErrorHandler) extends BaseInterpreter {

  import ECAException._
  import Interpreter.CStates

  protected case class IState(locals: Map[String, ECAValue], cstates: CStates) extends BaseInterpreterState {

    def value(name: String) = locals.get(name)

    def substitute(name: String, value: ECAValue, stackTrace: StackTrace) = IState(locals.updated(name, value), cstates)

    def enterFunction(name: String, arguments: Map[String, ECAValue])(block: IState => IState) = {
      val postFunState = block(IState(arguments, cstates))
      (IState(locals, postFunState.cstates), postFunState.locals.get(name))
    }

  }

  protected val functions = program.functions

  def run(funName: String, arguments: Seq[ECAValue], cstates: CStates = Map.empty): (CStates, ECAValue) = {
    val (intState, result) = evalFunction(funName, arguments, new IState(Map.empty, cstates),
      ECAException.newStackTraceBuilder(new FunName("interpret", Some("<internal>"))), None)
    (intState.cstates, result)
  }

  private def callComponentFunction(call: FunCall, arguments: Seq[ECAValue], state: IState, stackTrace: StackTraceBuilder): (IState, ECAValue) = {
    val compAlias = call.name.prefix.get
    val funName = call.name.name
    val comp = components.getOrElse(compAlias, errorHandler.fatalError(new ECAException(s"Undeclared component: '$compAlias'.", stackTrace.result(call))))
    val (cstate, result) = comp.eval(funName)(state.cstates.getOrElse(compAlias, comp.initialState).asInstanceOf[comp.CState], arguments)
    (IState(state.locals, state.cstates.updated(compAlias, cstate)), result)
  }

  override protected def evalExpression(expr: Expression, state: IState, stackTrace: StackTraceBuilder) = expr match {
    case call @ FunCall(qname, args) if qname.isPrefixed =>
      val (postArgsState, values) = evalExprList(args, state, stackTrace)
      callComponentFunction(call, values, postArgsState, stackTrace)
    case _ => super.evalExpression(expr, state, stackTrace)
  }

}

object Interpreter {

  type CStates = Map[String, ComponentModel#ComponentState]

  def main(args: Array[String]) {
    val fileName = args.headOption.getOrElse(sys.error("Missing file name."))
    val funName = args.tail.headOption.getOrElse("main")
    val progArgs = args.drop(2).map(a => ECAValue.bigIntToValue(BigInt(a))).toSeq

    val file = new File(fileName)
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler()//source = Some(source), file = Some(file))
    val program = new Parser(source, errorHandler).program()
    errorHandler.successOrElse("Parse errors encountered.")

    val components = ComponentModel.fromImports(program.imports, errorHandler)
    errorHandler.successOrElse("Errors loading components.")

    val checker = new SemanticAnalysis(program, components, errorHandler)
    checker.functionCallHygiene()
    checker.variableReferenceHygiene()
    errorHandler.successOrElse("Semantic errors; please fix these.")

    val interpreter = new Interpreter(program, components, errorHandler)
    println(interpreter.run(funName, progArgs))
  }

}
