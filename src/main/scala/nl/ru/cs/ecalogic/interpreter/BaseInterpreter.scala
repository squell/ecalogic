package nl.ru.cs.ecalogic
package interpreter

import ast._
import nl.ru.cs.ecalogic.util.{Position, ErrorHandler, Polynomial}
import model._

import scala.util.Try

/*
 Notes regarding paper, Figure 1 in specific:
 - Why does variable look-up not have any costs?
 - Paper interprets assignment as an expression; we interpret it as a statement.
 - Paper defines the result of a binary expression in terms of the CPU component; we use hardcoded binary operations.
   If we were to define binary operators as component functions in the CPU component, each operator could have its own
   costs as opposed to having Ee and Te as the constant costs for every binary expression.
 - For the sCF rule, args is passed to the delta function, but the delta function does not take any component function
   arguments.
 - Both the sCF and sLF rules (we assume; nowhere does it say what args actually is) have values instead of syntactic
   expressions as the arguments for the function calls. From the sE rule we assume that expressions are evaluated from
   left to right and that the resulting set of component states and timestamp are used for sCF and sLF.
 - In our implementation, expressions cannot be used as statements except for function calls. TODO?
 -
*/

trait BaseInterpreter {

  import ECAException._

  protected type IState <: BaseInterpreterState

  protected trait BaseInterpreterState {

    def value(name: String): Option[ECAValue]

    def substitute(name: String, value: ECAValue): IState

    def withFreshLocal(block: IState => IState): IState

  }

  protected val functions: Map[String, FunDef]
  protected val errorHandler: ErrorHandler
  protected val componentName: Option[String] = None

  protected def evalFunction(fun: BasicFunction, arguments: Seq[ECAValue], state: IState,
                             stackTrace: StackTraceBuilder, callPosition: Option[Position]): (IState, ECAValue) = {
    if (arguments.length != fun.arity) {
      errorHandler.fatalError(new ECAException(s"Function '${fun.name}' requires ${fun.arity} arguments; given: ${arguments.length}.", stackTrace.result(callPosition)))
    }
    val funName = new FunName(fun.name, if (fun.isComponent) componentName else None)
    val postFunState = state.withFreshLocal(evalStatement(fun.body, _, stackTrace.callFunction(funName, callPosition)))
    (postFunState, postFunState.value(fun.name).getOrElse(ECAValue.Zero))
  }

  protected def evalFunction(funName: String, arguments: Seq[ECAValue], state: IState,
                           stackTrace: StackTraceBuilder, callPosition: Option[Position]): (IState, ECAValue) = {
    val fun = functions.getOrElse(funName, errorHandler.fatalError(new ECAException(s"Undeclared function: '$funName'.", stackTrace.result(callPosition))))
    evalFunction(fun, arguments, state, stackTrace, callPosition)
  }

  protected def evalStatement(stmt: Statement, state: IState, stackTrace: StackTraceBuilder): IState = stmt match {
    case Skip() =>
      state
    case Assignment(variable, expr) =>
      val (postExprState, value) = evalExpression(expr, state, stackTrace)
      postExprState.substitute(variable, value)
    case f: FunCall =>
      val (postCallState, _) = evalExpression(f, state, stackTrace) // throw away the result
      postCallState
    case If(predicate, consequent, alternative) =>
      val (postCondState, condition) = evalExpression(predicate, state, stackTrace)
      if (condition) {
        evalStatement(consequent, postCondState, stackTrace)
      } else {
        evalStatement(alternative, postCondState, stackTrace)
      }
    case While(predicate, rf, consequent)  =>
      var (postCondState, condition) = evalExpression(predicate, state, stackTrace)
      val (_, bound) = rf.map(evalExpression(_, state, stackTrace)).unzip
      var counter = ECAValue.Zero
      while (condition) {
        counter += ECAValue.One
        bound.filter(counter > _).foreach { b =>
          errorHandler.warning(new ECAException(s"Number of loop iterations exceeds bound: $counter > $b.", stackTrace.result(stmt)))
        }
        val postBodyState = evalStatement(consequent, postCondState, stackTrace)
        evalExpression(predicate, postBodyState, stackTrace) match {
          case (s, c) =>
            postCondState = s
            condition = c
        }
      }
      state
    case Composition(statements) =>
      statements.foldLeft(state) {
        case (state, stmt) => evalStatement(stmt, state, stackTrace)
      }
    case Annotated(_, stmt) =>
      evalStatement(stmt, state, stackTrace)
    case ErrorNode() =>
      throw new Exception("This can never happen.")
  }

  protected def evalExprList(exprs: Seq[Expression], state: IState, stackTrace: StackTraceBuilder) =
    exprs.foldLeft((state, Seq.empty[ECAValue])) {
      case ((state, values), expr) =>
        val (postExprState, value) = evalExpression(expr, state, stackTrace)
        (postExprState, values :+ value)
    }


  protected def forceValue(poly: Polynomial): ECAValue = if(poly.isIntegral) poly.coef(Seq.empty) else errorHandler.fatalError(new ECAException(s"Cannot evaluate polynomial value '$poly'"))

  protected def evalExpression(expr: Expression, state: IState, stackTrace: StackTraceBuilder): (IState, ECAValue) = expr match {
    case Literal(value)       => (state, forceValue(value))
    case VarRef(name)         => (state, state.value(name).getOrElse(errorHandler.fatalError(new ECAException(s"Undeclared variable: '$name'", stackTrace.result(expr)))))

    case expr: BinaryExpression =>
      val (postLeftState, left)   = evalExpression(expr.left, state, stackTrace)
      val (postRightState, right) = evalExpression(expr.right, postLeftState, stackTrace)
      try {
        (postRightState, expr.operator(left, right))
      } catch {
        case e: ArithmeticException => errorHandler.fatalError(new ECAException(e.toString, e, stackTrace.result(expr)))
      }

    case FunCall(qname, args) if !qname.isPrefixed =>
      val (postArgsState, values) = evalExprList(args, state, stackTrace)
      evalFunction(qname.name, values, postArgsState, stackTrace, Some(expr.position))
    case _ =>
      (state, ECAValue.Zero)
      // Doesn't know how to deal with prefixed functions
  }

}


