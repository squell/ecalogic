package nl.ru.cs.ecalogic.util

import nl.ru.cs.ecalogic.parser.Position
import nl.ru.cs.ecalogic.SPLException
import java.io.PrintWriter
import scala.collection.mutable

trait ErrorHandler {
  def reset()
  
  def fatalError(exception: SPLException);
  def error(exception: SPLException)
  def warning(exception: SPLException)
  def errorOccurred: Boolean

  def tryCatch[T](s: => T): Option[T] = try Some(s) catch {
    case e: SPLException =>
      error(e)
      None
  }

  def tryCatch[T](s: => T, f: SPLException => T): T = try s catch {
    case e: SPLException =>
      error(e)
      f(e)
  }
}

class DefaultErrorHandler(maxErrorCount: Int = 10,
                          writer: PrintWriter = new PrintWriter(Console.err),
                          source: Option[String] = None) extends ErrorHandler {
  private var errorCount = 0

  private def printMessage(tpe: String, message: String, position: Option[Position]) {
    writer.print(tpe)
    writer.print(position.map(p => " at line %d, column %d".format(p.line, p.column)).mkString)
    writer.printf(":%n    %s%n", message)
    source.map(_ + "\n").zip(position).foreach { case (s, Position(l, c)) =>
      val line = s.lines.drop(l - 1).next()
      val trimmedLine = line.dropWhile(_ <= ' ')

      if (!trimmedLine.isEmpty) {
        val n = c - line.takeWhile(_ <= ' ').length

        writer.printf("%n    %s", trimmedLine)
        writer.printf("%n    %" + n + "s%n", "^")
      }
    }
    writer.flush()
  }

  def reset() {
    errorCount = 0
  }

  def errorOccurred: Boolean = errorCount > 0

  def fatalError(exception: SPLException) {
    printMessage("Fatal error", exception.message, exception.position)
    throw new SPLException("Fatal error occurred", exception);
  }
  
  def error(exception: SPLException) {
    printMessage("Error", exception.message, exception.position)
    errorCount += 1
    if (maxErrorCount > 0 && errorCount >= maxErrorCount) {
      throw new SPLException("Maximum number of errors reached")
    }
  }

  def warning(exception: SPLException) {
    printMessage("Warning", exception.message, exception.position)
  }
}

class CachingErrorHandler(val output: ErrorHandler = new DefaultErrorHandler) extends ErrorHandler {
  private val errors = mutable.PriorityQueue[(SPLException, Boolean)]()

  def reset() {
    errors.clear()
  }

  def errorOccurred: Boolean = errors.exists(!_._2)

  def fatalError(exception: SPLException) {
    flush()
    output.fatalError(exception)
  }

  def error(exception: SPLException) {
    errors += ((exception, false))
  }

  def warning(exception: SPLException) {
    errors += ((exception, true))
  }

  def flush() {
    errors.dequeueAll.reverse.foreach { case (e, w) =>
      if (w)
        output.warning(e)
      else
        output.error(e)
    }
  }
}