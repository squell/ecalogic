package nl.ru.cs.ecalogic.parser

case class Position(line: Int, column: Int) extends Ordered[Position] {
  def compare(that: Position) = {
    val lineDiff = line - that.line
    if (lineDiff == 0) column - that.column
    else lineDiff
  }

  override def toString = s"$line:$column"
}

trait Positional {
  def position: Option[Position]
}
