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

  protected type IState = InterpreterState

  protected class InterpreterState(val state: GlobalState, locals: Map[String, ECAValue] = Map.empty) extends BaseInterpreterState {

    def value(name: String) = locals.get(name)

    def substitute(name: String, value: ECAValue) = new InterpreterState(state, locals.updated(name, value))

    def withFreshLocal(block: IState => IState) = {
      block(this)
      this
    }

  }

  protected val functions = program.functions

  def run(funName: String, arguments: Seq[ECAValue], state: GlobalState = GlobalState.initial(components)): (GlobalState, ECAValue) = {
    val (intState, result) = evalFunction(funName, arguments, new InterpreterState(state), ECAException.newStackTraceBuilder(new FunName("interpret", Some("<internal>"))), None)
    (intState.state, result)
  }

}

object Interpreter extends App {

  val fileName = config.Options(args).headOption.getOrElse("program.eca")
  val noCPU = args.last == "nocpu"

  val file = new File(fileName)
  val source = Source.fromFile(file).mkString
  val errorHandler = new DefaultErrorHandler()//source = Some(source), file = Some(file))
  val program = new Parser(source, errorHandler).program()
  errorHandler.successOrElse("Parse errors encountered.")

  import model.examples._
  import model.examples.DemoComponents._
  val components = Map("Stub"->StubComponent, "BAD"->BadComponent, "Sensor"->Sensor, "Radio"->Radio, if(noCPU) "Stub"->StubComponent else "CPU"->CPU)

  val checker = new SemanticAnalysis(program, components, errorHandler)
  checker.functionCallHygiene()
  checker.variableReferenceHygiene()
  errorHandler.successOrElse("Semantic errors; please fix these.")

  val interpreter = new Interpreter(program, components, errorHandler)
  println(interpreter.run("program", Seq(ECAValue.Zero)))

}
