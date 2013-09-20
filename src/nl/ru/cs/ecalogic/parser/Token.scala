package nl.ru.cs.ecalogic.parser

sealed trait TokenTemplate

sealed abstract class WildcardTemplate(val name: String) extends TokenTemplate {
  override def toString = "<" + name + ">"
}

sealed trait Token extends TokenTemplate {
  def ignorable = false
  def matches(template: TokenTemplate): Boolean = this == template
}

sealed abstract class FixedToken(fixedValue: String) extends Token {
  override def toString = "'" + fixedValue + "'"
}

sealed abstract class VariableToken[T](wildcard: WildcardTemplate) extends Token {
  def value: T

  override def matches(template: TokenTemplate) = if (super.matches(template)) true else template match {
    case `wildcard` => true
    case _          => false
  }

  override def toString = "<" + wildcard.name + " : '" + value + "'>"
}

object Wildcards {

  case object IntLiteral                     extends WildcardTemplate("integer-literal")
  case object Identifier                     extends WildcardTemplate("identifier")
  case object Comment                        extends WildcardTemplate("comment")
  case object Whitespace                     extends WildcardTemplate("whitespace")
  case object Unknown                        extends WildcardTemplate("unknown")

}

object Tokens {

  case object Function                       extends FixedToken("function")
  case object Return                         extends FixedToken("return")
  case object End                            extends FixedToken("end")
  
  case object If                             extends FixedToken("if")
  case object Then							             extends FixedToken("then")
  case object Else                           extends FixedToken("else")
  
  case object While                          extends FixedToken("while")
  case object Upto                           extends FixedToken("upto")
  case object Do                             extends FixedToken("do")
  
  case object Skip                           extends FixedToken("skip")

  final case class IntLiteral(value: BigInt) extends VariableToken[BigInt](Wildcards.IntLiteral)
  final case class Identifier(value: String) extends VariableToken[String](Wildcards.Identifier)

  case object Assign                         extends FixedToken(":=")

  case object Plus                           extends FixedToken("+")
  case object Minus                          extends FixedToken("-")
  case object Multiply                       extends FixedToken("*")

  case object EQ                             extends FixedToken("=")
  case object LT                             extends FixedToken("<")
  case object GT                             extends FixedToken(">")
  case object LE                             extends FixedToken("<=")
  case object GE                             extends FixedToken(">=")
  case object NE                             extends FixedToken("<>")

  case object And                            extends FixedToken("and")
  case object Or                             extends FixedToken("or")

  case object LParen                         extends FixedToken("(")
  case object RParen                         extends FixedToken(")")

  case object Comma                          extends FixedToken(",")
  case object Semicolon                      extends FixedToken(";")
  
  case object ColonColon                     extends FixedToken("::")

  final case class Comment(value: String)    extends VariableToken[String](Wildcards.Comment)    { override def ignorable = true }
  final case class Whitespace(value: String) extends VariableToken[String](Wildcards.Whitespace) { override def ignorable = true }
  case object EndOfFile                      extends FixedToken("")  { override def toString = "<end-of-file>" }

  final case class Unknown(value: Char)      extends VariableToken[Char](Wildcards.Unknown)

}

