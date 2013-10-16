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

package nl.ru.cs.ecalogic
package util

import scala.collection.mutable

import java.io.{File, PrintWriter}

/** Base trait for error handlers.
  *
  * @author Jascha Neutelings
  */
trait ErrorHandler {
  /** Resets the error handler to its original state. */
  def reset()

  /** Reports a fatal error.
    *
    * @param exception error to report
    */
  def fatalError(exception: ECAException)

  /** Reports an error.
    *
    * @param exception error to report
    */
  def error(exception: ECAException)

  /** Reports a warning.
    *
    * @param exception error to report
    */
  def warning(exception: ECAException)

  /** Returns whether an error has occurred. */
  def errorOccurred: Boolean

  /** Tries to evaluate the given expression and optionally returns the result and otherwise reports an error.
    *
    * @param  expression expression to evaluate
    * @tparam T          type of the expression
    * @return            optional result
    */
  def tryCatch[T](expression: => T): Option[T] = try Some(expression) catch {
    case e: ECAException =>
      error(e)
      None
  }

  /** Does nothing if no errors occurred; otherwise, throws an exception 
    * 
    * @param  complaint explanation of the error condition
    */
  def successOrElse(complaint: String) {
    if(errorOccurred) throw new ECAException(complaint)
  }

}

/** Default implementation for error handlers.
  *
  * Prints error messages to a `java.io.PrintWriter`. Counts the number of errors reported and throws an exception
  * if the maximum number is exceeded. If file name and/or source string are provided more informative messages will
  * be generated. Throws an exception on a fatal error after reporting it first.
  *
  * @param maxErrorCount maximum number of error messages
  * @param writer        output
  * @param source        optional input
  * @param file          optional file name
  *
  * @author Jascha Neutelings
  */
class DefaultErrorHandler(maxErrorCount: Int = 10,
                          writer: PrintWriter = new PrintWriter(Console.err),
                          source: Option[String] = None,
                          file: Option[File] = None) extends ErrorHandler {
  private var errorCount = 0

  private def printMessage(tpe: String, message: String, position: Option[Position]) {
    writer.print(tpe)
    file.foreach(f => writer.print(s" in '${f.getAbsolutePath}'"))
    position.foreach(p => writer.print(s" at line ${p.line}, column ${p.column}"))
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

  def fatalError(exception: ECAException) {
    printMessage("Fatal error", exception.message, exception.position)
    errorCount += 1
    throw new ECAException("Fatal error occurred", exception)
  }

  def error(exception: ECAException) {
    printMessage("Error", exception.message, exception.position)
    errorCount += 1
    if (maxErrorCount > 0 && errorCount >= maxErrorCount) {
      throw new ECAException("Maximum number of errors reached")
    }
  }

  def warning(exception: ECAException) {
    printMessage("Warning", exception.message, exception.position)
  }

}

/** Error handler that caches error and prints them on demand in the order they occurred.
  *
  * Wraps around an existing error handler.
  *
  * @param output error handler to wrap
  *
  * @author Jascha Neutelings
  */
class CachingErrorHandler(val output: ErrorHandler = new DefaultErrorHandler) extends ErrorHandler {
  private val errors = mutable.PriorityQueue.empty[(ECAException, Boolean)]

  def reset() {
    errors.clear()
    output.reset()
  }

  def errorOccurred: Boolean = output.errorOccurred || errors.exists(!_._2)

  def fatalError(exception: ECAException) {
    flush()
    output.fatalError(exception)
  }

  def error(exception: ECAException) {
    errors += ((exception, false))
  }

  def warning(exception: ECAException) {
    errors += ((exception, true))
  }

  /** Flushes all error messages to the underlying error handler and clears the buffer. */
  def flush() {
    errors.dequeueAll.reverse.foreach { case (e, w) =>
      if (w)
        output.warning(e)
      else
        output.error(e)
    }
  }

}
