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
package model

import ast._
import parser.ModelParser
import util.DefaultErrorHandler

import scala.collection.mutable
import scala.io.Source

import java.io.File

trait ECMModel extends ComponentModel {

  class CState(val elements: Map[String, ECAValue]) extends ComponentState {

    protected def update(newElements: Map[String, ECAValue]) = ???

  }

  val initialState = new CState(Map.empty)
  private val elements                 = Map.empty[String, ECAValue].withDefault(n => throw new ECAException(s"Undefined element: '$n'."))
  private val compFunctions            = Map.empty[String, CompFunDef]
  private val functions                = Map.empty[String, FunDef]
//  private val tdFunction: TDFunction   = functions.get("td").map(evalFunction(_, )
//  private var lubFunction: LUBFunction = super.lub
//  private var phiFunction: PHIFunction = super.phi
//
  private def evalFunction(fun: BasicFunction, arguments: Seq[ECAValue]): ECAValue = {
    0
  }

}

object ECMModel {

  def fromNode(node: Component): ECMModel = ???

  def fromFile(file: File): ECMModel = {
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new ModelParser(source, errorHandler)
    val node = parser.component()
    errorHandler.successOrElse("Parsing failed.")
    fromNode(node)
  }

}
