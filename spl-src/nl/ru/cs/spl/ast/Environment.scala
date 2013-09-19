package nl.ru.cs.spl.ast

import scala.collection.mutable
import scala.util.Random

abstract class Environment (val parent: Option[Environment] = None) {
  private val variables = mutable.Map[String, (VarDecl, VarInfo)]()
  private val renamed   = mutable.Map[String, String]()
  private val children  = mutable.Set[Environment]()

  //private def isVariableShadowing(n: String): Boolean =  parent.map(p => p.variables.contains(n) || p.isVariableShadowing(n)).getOrElse(false)

  // def name: String

  def getVariableAndInfo(name: String): Option[(VarDecl, VarInfo)] = variables.get(name).orElse(parent.flatMap(_.getVariableAndInfo(name)))

  def getVariable(n: String) = getVariableAndInfo(n).map(_._1)

  def updateVariableInfo(n: String)(f: VarInfo => VarInfo) {
    variables.get(n) match {
      case Some((varDecl, varInfo)) => variables(n) = (varDecl, f(varInfo))
      case None                     => parent.foreach(_.updateVariableInfo(n)(f))
    }
  }

  //def getVariableInfo(n: String) = getVariableEntry(n).map(_._2)

  def resolveVariableName(n: String): String = renamed.getOrElse(n, parent.map(_.resolveVariableName(n)).getOrElse(n))

  def declareVariable(v: VarDecl): Option[String] = {
    if (variables.contains(resolveVariableName(v.name))) None
    else {
      val newName = getUniqueName(v.name)
      variables(newName) = (v.copy(name = newName), VarInfo(v.name, isGlobal))
      if (newName != v.name) renamed(v.name) = newName
      Some(newName)
    }
  }

  def getUniqueName(prefix: String): String

//  def declareTemporaryVariable(tpe: Type): VarDecl = {
//    val tempName = getUniqueName("$temp")
//    val tempDecl = VarDecl(tempName, tpe)
//    variables(tempName) = (tempDecl, VarInfo("?", temporary = true))
//    tempDecl
//  }

  //def createTemporaryVariable(tpe: Type) = VarDecl(getUniqueName(""), tpe)

  def collectVariables(recursive: Boolean): Iterator[VarDecl] =
    if (recursive)
      variables.valuesIterator.map(_._1) ++ children.flatMap(_.collectVariables(recursive))
    else
      variables.valuesIterator.map(_._1)

  def getFunction(n: String): Option[FunDecl]

  def declareFunction(f: FunDecl): Boolean

  def updateFunctionInfo(n: String)(f: FunInfo => FunInfo)

  def getFunctionInfo(n: String): Option[FunInfo]

  def isTypeVarDefined(t: TypeParam): Boolean

  def collectTypeVariables: Iterator[TypeParam] = Iterator.empty

  def declareTypeVar(t: TypeParam) { }

//  def create(n: String): Environment = {
//    val env = new Environment.Local(this, n)
//    children(n) = env
//    env
//  }

  //def create(): Environment = create((if (isGlobal) "" else name + "$") + children.size)

  def create: Environment

  //def getEnv(n: String) = children(n)

  def isGlobal = parent.isEmpty
}

object Environment {
  def create: Environment = new Global

  private class Local(parent: Environment) extends Environment(Some(parent)) {
    require(parent != null)


    def declareFunction(f: FunDecl) = parent.declareFunction(f)

    def getFunction(n: String) = parent.getFunction(n)

    def updateFunctionInfo(n: String)(f: FunInfo => FunInfo) {
      parent.updateFunctionInfo(n)(f)
    }

    def getFunctionInfo(n: String)= parent.getFunctionInfo(n)

    def isTypeVarDefined(t: TypeParam) = parent.isTypeVarDefined(t)

    def getUniqueName(prefix: String) = parent.getUniqueName(prefix)

    def create: Environment = {
      val res = new Local(this)
      children += res
      res
    }
  }

  private class Function(parent: Global) extends Local(parent) {
    private val typeVars = mutable.Set[TypeParam]()
    private val suffixes  = mutable.Map("" -> 0)

    override def getUniqueName(prefix: String) = {
      val uniqueName = suffixes.get(prefix).map(prefix + "$" + _).getOrElse(prefix)
      suffixes(prefix) = suffixes.get(prefix).map(_ + 1).getOrElse(1)
      uniqueName
    }

    override def declareTypeVar(t: TypeParam) {
      typeVars += t
    }

    override def isTypeVarDefined(t: TypeParam) = typeVars(t)

    override def collectTypeVariables = typeVars.iterator
  }

  private class Global extends Environment {
    private val functions = mutable.Map[String, (FunDecl, FunInfo)]()
    private val suffixes  = mutable.Map("" -> 0)

    PredefinedFunctions.register(this)

    //def name = "<isGlobal>"

    def getFunction(n: String) = functions.get(n).map(_._1)

    def updateFunctionInfo(n: String)(f: FunInfo => FunInfo) {
      functions.get(n) match {
        case Some((funDecl, funInfo)) => functions(n) = (funDecl, f(funInfo))
        case None                     =>
      }
    }

    def getFunctionInfo(n: String) = functions.get(n).map(_._2)

    def declareFunction(f: FunDecl) =
      if (functions.contains(f.name)) false
      else {
        functions(f.name) = (f, FunInfo.default); true
      }

    def isTypeVarDefined(t: TypeParam) = false

    def getUniqueName(prefix: String) = {
      val uniqueName = suffixes.get(prefix).map(prefix + "$" + _).getOrElse(prefix)
      suffixes(prefix) = suffixes.get(prefix).map(_ + 1).getOrElse(1)
      uniqueName
    }

    def create: Environment = {
      val res = new Function(this)
      children += res
      res
    }
  }
}



trait Symbol {
  def name: String
}


trait VarType

object VarType {
  case object Param extends VarType
  case object Local extends VarType
  case object Global extends VarType
  case object Result extends VarType
}

case class VarDecl(name: String, tpe: Type, varType: VarType) extends Symbol

case class FunDecl(name: String, resultType: Type, paramTypes: Seq[Type]) extends Symbol

case class VarInfo(originalName: String, global: Boolean)

case class FunInfo(references: Int, sideEffects: Boolean)

object FunInfo {
  def default = FunInfo(0, sideEffects = false)
}
