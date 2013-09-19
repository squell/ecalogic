package nl.ru.cs.spl.parser

import nl.ru.cs.spl.util.{DefaultErrorHandler, ErrorHandler}
import nl.ru.cs.spl.SPLException
import nl.ru.cs.spl.parser.tokens._
import scala.io.Source

class Lexer(private var input: String, protected val errorHandler: ErrorHandler = new DefaultErrorHandler()) {
  private var line = 1
  private var column = 1

  private def position = Position(line, column)

  private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isIdHead(c: Char) = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

  private def isIdTail(c: Char) = isIdHead(c) || isDigit(c) || c == '_'

  private def isWhitespace(c: Char) = c == ' ' || c == '\t' || c == '\r' || c == '\n'

  private def consume(length: Int) {
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
        case '+' => (Plus, 1)
        case '-' => (Minus, 1)
        case '*' => (Multiply, 1)
        case '%' => (Modulo, 1)
        case '/' =>
          if (lookahead('/')) {
            val value = input.drop(2).takeWhile(_ != '\n')

            (Comment(value.trim), value.length + 3)
          } else if (lookahead('*')) {
            if (input.indexOf("*/", 2) < 0) errorHandler.fatalError(new SPLException("Unterminated comment.", pos))
            val (value, _) = input.drop(2).zip(input.drop(3)).takeWhile(_ != ('*', '/')).unzip

            (Comment(value.mkString.trim), value.length + 4)
          } else (Divide, 1)
        case '=' => if (lookahead('=')) (EQ, 2) else (Assign, 1)
        case '<' => if (lookahead('=')) (LE, 2) else (LT, 1)
        case '>' => if (lookahead('=')) (GE, 2) else (GT, 1)
        case '!' => if (lookahead('=')) (NE, 2) else (Not, 1)
        case '&' => if (lookahead('&')) (And, 2) else (Unknown(ch), 1)
        case '|' => if (lookahead('|')) (Or, 2) else (Unknown(ch), 1)
        case ':' => (Cons, 1)
        case '(' => (LParen, 1)
        case ')' => (RParen, 1)
        case '[' => (LSquare, 1)
        case ']' => (RSquare, 1)
        case '{' => (LCurly, 1)
        case '}' => (RCurly, 1)
        case ',' => (Comma, 1)
        case ';' => (Semicolon, 1)
        case c =>
          if (isDigit(c)) {
            val value = input.takeWhile(isDigit)
            (IntLiteral(BigInt(value)), value.length)
          } else if (isIdHead(c)) {
            val value = input.takeWhile(isIdTail)
            val token = value match {
              case "Void" => Void
              case "Int" => Int
              case "Bool" => Bool
              case "if" => If
              case "else" => Else
              case "while" => While
              case "return" => Return
              case "True" => True
              case "False" => False
              case v => Identifier(v)
            }
            (token, value.length)
          } else if (isWhitespace(c)) {
            val value = input.takeWhile(isWhitespace)
            (Whitespace(value), value.length)
          } else (Unknown(c), 1)
      }
    }
    consume(length)
    (token, pos)
  }
}

object Lexer extends App {
  val source = Source.fromFile("Example0_Factorial.spl").mkString
  val lexer = new Lexer(source)
  var (token, _) = lexer.next()
  while (token != EndOfFile) {
    if (!token.ignorable) println(token)
    token = lexer.next()._1
  }
}