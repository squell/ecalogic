package nl.ru.cs.ecalogic.parser

sealed abstract class Token {
  def matches(that: Token) = this == that
}

sealed abstract class FixedToken(fixedValue: String) extends Token {
  override def toString = "'" + fixedValue + "'"
}

sealed abstract class Keyword(val keyword: String) extends FixedToken(keyword)

sealed abstract class VariableToken[T](name: String) extends Token {
  def value: T

  override def toString = "<" + name + " : '" + value + "'>"
}

object Tokens {

  case object Function                 extends Keyword("function")
  case object Return                   extends Keyword("return")
  case object End                      extends Keyword("end")
  
  case object If                       extends Keyword("if")
  case object Then							       extends Keyword("then")
  case object Else                     extends Keyword("else")
  
  case object While                    extends Keyword("while")
  case object Upto                     extends Keyword("upto")
  case object Do                       extends Keyword("do")
  
  case object Skip                     extends Keyword("skip")

  case class Numeral(value: BigInt)    extends VariableToken[BigInt]("numeral")
  case class Identifier(value: String) extends VariableToken[String]("identifier")

  case object Assign                   extends FixedToken(":=")

  case object Plus                     extends FixedToken("+")
  case object Minus                    extends FixedToken("-")
  case object Multiply                 extends FixedToken("*")

  case object EQ                       extends FixedToken("=")
  case object LT                       extends FixedToken("<")
  case object GT                       extends FixedToken(">")
  case object LE                       extends FixedToken("<=")
  case object GE                       extends FixedToken(">=")
  case object NE                       extends FixedToken("<>")

  case object And                      extends FixedToken("and")
  case object Or                       extends FixedToken("or")

  case object LParen                   extends FixedToken("(")
  case object RParen                   extends FixedToken(")")

  case object Comma                    extends FixedToken(",")
  case object Semicolon                extends FixedToken(";")
  
  case object ColonColon               extends FixedToken("::")

  case class Comment(value: String)    extends VariableToken[String]("comment")
  case class Whitespace(value: String) extends VariableToken[String]("whitespace")
  case object EndOfFile                extends Token { override def toString = "<end-of-file>" }

  case class Unknown(value: Char)      extends VariableToken[Char]("unknown")

  object Identifier                    extends Token {
    override def matches(that: Token) = super.matches(that) || (that match  {
      case Identifier(_) => true
      case _             => false
    })
  }

  object Numeral                       extends Token {
    override def matches(that: Token) = super.matches(that) || (that match  {
      case Numeral(_) => true
      case _          => false
    })
  }

  object KeywordOrIdentifier           extends Token {
    override def matches(that: Token) = super.matches(that) || (that match  {
      case Identifier(_) => true
      case _: Keyword    => true
      case _             => false
    })

    def unapply(t: Token): Option[String] = t match {
      case Tokens.Identifier(n) => Some(n)
      case k: Keyword           => Some(k.keyword)
      case _                    => None
    }
  }

}
