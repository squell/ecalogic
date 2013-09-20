package nl.ru.cs.ecalogic.parser

import nl.ru.cs.ecalogic.util.{DefaultErrorHandler, ErrorHandler}
import nl.ru.cs.ecalogic.SPLException
import nl.ru.cs.ecalogic.parser.Tokens._
import scala.io.Source

class Lexer(private var input: String,
            protected val errorHandler: ErrorHandler = new DefaultErrorHandler()) {

  private var line = 1
  private var column = 1

  private def position = Position(line, column)

  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isIdHead(c: Char) = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

  private def isIdTail(c: Char) = isIdHead(c) || isDigit(c) || c == '_'

  private def isWhitespace(c: Char) = c == ' ' || c == '\t' || c == '\r' || c == '\n'

  private def consume(length: Int) { // Kan mogelijk efficiënter?
    input.take(length).foreach(c =>
      if (c == '\n') {
        line += 1; column = 1
      } else column += 1)
    input = input.drop(length)
  }

  def next(): (Token, Position) = {
    def lookahead(c: Char) = input.length() > 1 && input.charAt(1) == c

    val pos = position
    val character = if (input.isEmpty) None else Some(input.head)
    val (token: Token, length: Int) = character match {
      case None => (EndOfFile, 0)
      case Some(ch) => ch match {
        case '+'                   => (Plus, 1)
        case '-'                   => (Minus, 1)
        case '*'                   => (Multiply, 1)

        case '='                   => (EQ, 1)
        case '<' if lookahead('=') => (LE, 2) 
        case '<' if lookahead('>') => (NE, 2)
        case '<'                   => (LT, 1)
        case '>' if lookahead('=') => (GE, 2)
        case '>'                   => (GT, 1)
        case ':' if lookahead(':') => (ColonColon, 2)
        case ':' if lookahead('=') => (Assign, 2)
        
        case '/' if lookahead('/') =>
          val value = input.drop(2).takeWhile(_ != '\n')
          (Comment(value.trim), value.length + 3)

        case '(' if lookahead('*') =>
	        if (input.indexOf("*)", 2) < 0) errorHandler.fatalError(new SPLException("Unterminated comment.", pos))
	        val (value, _) = input.drop(2).zip(input.drop(3)).takeWhile(_ != ('*', ')')).unzip
	
	        (Comment(value.mkString.trim), value.length + 4)

        case '('                   => (LParen, 1)
        case ')'                   => (RParen, 1)
        case ','                   => (Comma, 1)
        case ';'                   => (Semicolon, 1)

        case d if isDigit(d)       =>
          val value = input.takeWhile(isDigit)
          (IntLiteral(BigInt(value)), value.length)

        case h if isIdHead(h)      =>
          val value = input.takeWhile(isIdTail)
          val token = value match {
            case "function" => Function
            case "return"   => Return
            case "end"      => End
            case "if"       => If
            case "then"     => Then
            case "else"     => Else
            case "while"    => While
            case "upto"     => Upto
            case "do"       => Do
            case "skip"     => Skip
            case "and"      => And
            case "or"       => Or

            case v          => Identifier(v)
          }
          (token, value.length)

        case w if isWhitespace(w)  =>
          val value = input.takeWhile(isWhitespace)
          (Whitespace(value), value.length)

        case c                     => (Unknown(c), 1)
      }
    }
    consume(length)
    (token, pos)
  }
}

object Lexer {

  def main(args: Array[String]) {
    val source = Source.fromFile(args.headOption.getOrElse("test.eca")).mkString
    val lexer = new Lexer(source, new DefaultErrorHandler(source = Some(source)))
    var (token, _) = lexer.next()
    while (token != EndOfFile) {
      if (!token.ignorable) println(token)
      token = lexer.next()._1
    }
  }

}