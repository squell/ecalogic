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

import parser.Parser
import analysis.{SemanticAnalysis, EnergyAnalysis}
import config.Options
import util.DefaultErrorHandler

import scala.io.Source

import java.io.File

import model.ComponentModel

object run {

  def main(args: Array[String]) {
    val fileName = Options(args).headOption.getOrElse("program.eca")
    val defaultErrorHandler = new DefaultErrorHandler
    try {
      val file = new File(fileName)
      val source = defaultErrorHandler.handle(Source.fromFile(file).mkString)
      val errorHandler = new DefaultErrorHandler(sourceText = Some(source), sourceURI = Some(file.toURI))

      val program = errorHandler.handleBlock("One or more errors occurred during parsing. Aborted.") {
        val parser = new Parser(source, errorHandler)
        parser.program()
      }

      errorHandler.handleBlock("One or more errors occurred during semantic analysis. Aborted.") {
        val checker = new SemanticAnalysis(program, errorHandler)
        checker.functionCallHygiene()
        checker.variableReferenceHygiene()
      }

      //val stub = ECMModel.fromFile("doc/examples/Stub.ecm")
      //val components = Set[ComponentModel](stub)
      //val components = Set(StubComponent, BadComponent, Sensor, Radio, if(config.Options.noCPU) StubComponent else CPU)

      val components = errorHandler.handleBlock("One or more errors occurred while loading components. Aborted.") {
        ComponentModel.fromImports(program.imports, errorHandler)
      }

      val finalState = errorHandler.handleBlock("One or more errors occurred during energy analysis. Aborted.") {
        val consumptionAnalyser = new EnergyAnalysis(program, components, errorHandler)
        consumptionAnalyser.analyse()
      }
      println(finalState)

      println(s"Analysis of '${file.getAbsolutePath}' was successful.")
    } catch {
      case _: ECAException          =>
        sys.exit(1)
      case e: Exception =>
        Console.err.println("Oops. An exception seems to have escaped.")
        e.printStackTrace()
        sys.exit(2)
    }
  }

}
