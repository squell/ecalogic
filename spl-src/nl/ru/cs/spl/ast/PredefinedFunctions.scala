package nl.ru.cs.spl.ast

object PredefinedFunctions {
  private val functionNames = Set("print", "println", "printchar", "isEmpty", "head", "tail", "fst", "snd")

  def isPredefined(name: String) = functionNames(name)

  def register(env: Environment) {
    env.declareFunction(FunDecl("print"    , VoidType()                  , Seq(TypeArg(Left("t")))))
    env.declareFunction(FunDecl("println"  , VoidType()                  , Seq.empty))
    env.declareFunction(FunDecl("printchar", VoidType()                  , Seq(IntType())))
    env.declareFunction(FunDecl("isEmpty"  , BoolType()                  , Seq(ListType(TypeArg(Left("t"))))))
    env.declareFunction(FunDecl("head"     , TypeArg(Left("t"))          , Seq(ListType(TypeArg(Left("t"))))))
    env.declareFunction(FunDecl("tail"     , ListType(TypeArg(Left("t"))), Seq(ListType(TypeArg(Left("t"))))))
    env.declareFunction(FunDecl("fst"      , TypeArg(Left("a"))          , Seq(TupleType(TypeArg(Left("a")), TypeArg(Left("b"))))))
    env.declareFunction(FunDecl("snd"      , TypeArg(Left("b"))          , Seq(TupleType(TypeArg(Left("a")), TypeArg(Left("b"))))))
  }

  val optimize: PartialFunction[(String, Seq[Expression]), Expression] = {
    case ("isEmpty", Seq(Literal(NilValue)))         => Literal(False)
    case ("isEmpty", Seq(Literal(_)))                => Literal(True)
    case ("head"   , Seq(Literal(ConsValue(x, _))))  => Literal(x)
    case ("tail"   , Seq(Literal(ConsValue(_, xs)))) => Literal(xs)
    case ("fst"    , Seq(Literal(TupleValue(a, _)))) => Literal(a)
    case ("snd"    , Seq(Literal(TupleValue(_, b)))) => Literal(b)
  }

  def checkValidity(name: String, operands: Seq[Expression]): Option[String] = (name, operands) match {
    case ("head"   , Seq(Literal(NilValue))) => Some("Applying 'head' to an empty list is undefined behaviour")
    case ("tail"   , Seq(Literal(NilValue))) => Some("Applying 'tail' to an empty list is undefined behaviour")
    case _                                   => None
  }
}
