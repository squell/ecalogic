/*
 * ecalogic: a tool for performing energy consumption analysis.
 *
 * Copyright (c) 2013, J. Neutelings, D. Peelen, M. Schoolderman
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 *   Neither the name of the Radboud University Nijmegen nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package nl.ru.cs.ecalogic.util

import nl.ru.cs.ecalogic.parser.Position
import nl.ru.cs.ecalogic.SPLException
import java.io.PrintWriter
import scala.collection.mutable

trait ErrorHandler {
  def reset()

  def fatalError(exception: SPLException)
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
    writer.print(position.fold("")(p => s" at line ${p.line}, column ${p.column}"))
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
    throw new SPLException("Fatal error occurred", exception)
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