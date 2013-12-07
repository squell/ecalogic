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

import ast.FunName
import util.{Positional, Position}

import ECAException.StackTrace

/** Base class for ecalogic exceptions.
  *
  * @param message  exception message
  * @param position optional position in source
  * @param cause    optional cause
  *
  * @author Jascha Neutelings
  */
class ECAException(val message: String,
                   val position: Option[Position] = None,
                   val cause: Option[Throwable] = None,
                   val stackTrace: StackTrace = Seq.empty,
                   val reported: Boolean = false) extends RuntimeException(message, cause.orNull)
with Ordered[ECAException] {

  /** @see [[nl.ru.cs.ecalogic.ECAException]] */
  def this(message: String, position: Position) = this(message, Some(position))

  /** @see [[nl.ru.cs.ecalogic.ECAException]] */
  def this(message: String, positional: Positional) = this(message, positional.position)

  /** @see [[nl.ru.cs.ecalogic.ECAException]] */
  def this(message: String, cause: Throwable) = this(message, None, Option(cause))

  /** @see [[nl.ru.cs.ecalogic.ECAException]] */
  def this(message: String, stackTrace: StackTrace) = this(message, stackTrace.headOption.flatMap(_._2), None, stackTrace)

  def this(message: String, cause: Throwable, stackTrace: StackTrace) = this(message, stackTrace.headOption.flatMap(_._2), Some(cause), stackTrace)

  def compare(that: ECAException): Int = position.compare(that.position)

  def markReported = new ECAException(message, position, cause, stackTrace, true)

}

object ECAException {

  type StackTrace = Seq[(FunName, Option[Position])]

  class StackTraceBuilder private[ECAException](current: FunName, elements: StackTrace) {

    def callFunction(name: FunName, position: Option[Position] = None) = new StackTraceBuilder(name, (current, position) +: elements)

    def result(position: Option[Position] = None): StackTrace  = (current, position) +: elements
    def result(positional: Positional): StackTrace             = result(Some(positional.position))

  }

  def newStackTraceBuilder(name: FunName): StackTraceBuilder = new StackTraceBuilder(name, Seq.empty)

}
