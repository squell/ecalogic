package nl.ru.cs.spl

import nl.ru.cs.spl.parser.{Position, Positional}

class SPLException(val message: String,
                   val position: Option[Position] = None,
                   val cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)
                                                     with Ordered[SPLException] {
  def this(message: String, positional: Positional) = this(message, positional.position)
  def this(message: String, position: Position) = this(message, Some(position))
  def this(message: String, cause: Throwable) = this(message, None, Option(cause))

  def withPosition(position: Position) = new SPLException(message, Some(position))
  def withPosition(positional: Positional) = new SPLException(message, positional.position)

  def compare(that: SPLException): Int = position.compare(that.position)
}