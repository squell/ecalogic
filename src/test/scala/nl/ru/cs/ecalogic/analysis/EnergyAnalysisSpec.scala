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
package analysis

import org.scalatest.{FlatSpec, Matchers}
import java.io.File
import scala.io.Source
import parser.{Parser, Lexer}
import parser.Lexer.Tokens
import model._
import model.examples.BadComponent
import model.examples.StubComponent
import model.examples.DemoComponents._
import util._
import scala.collection.immutable.SortedMap

/**
 * @Author: Dorus Peelen
 */
class EnergyAnalysisSpec extends FlatSpec with Matchers {

  behavior of "The Energy Analysis"

  new File("doc/examples").listFiles().withFilter(_.getName.endsWith(".eca")).foreach { file =>
    //parse(f)
    val source = Source.fromFile(file).mkString
    val lexer = new Lexer(source)

    var (token, _) = lexer.next()
    while (token != Tokens.EndOfFile) {
      token match {
        case Tokens.Comment(c) if c.startsWith("expect:") =>
          val comment = c.substring(8)
          val errorHandler = new DefaultErrorHandler(sourceText = Some(source), sourceURI = Some(file.toURI))

          val parser = new Parser(source, errorHandler)
          val program = parser.program()

          val dsl_components = Map("Stub"->StubComponent, "BUG"->BadComponent, "Sensor"->Sensor, "Radio"->Radio, "CPU"->CPU)

          it should s"succeed with Scala components for ${file.getName}" in {
            val analyzer = new EnergyAnalysis(program, dsl_components, errorHandler)
            val result = analyzer.analyse().mapValues(_.e)
            val resultString = (SortedMap.empty[String, Polynomial] ++ result._1, result._2).toString
            resultString should equal (comment)
          }

          it should s"succeed with imported components for ${file.getName}" in {
            val imported_components = ComponentModel.fromImports(program.imports, errorHandler)
            errorHandler.successOrElse("Error importing")
            val analyzer = new EnergyAnalysis(program, dsl_components++imported_components, errorHandler)
            val result = analyzer.analyse().mapValues(_.e)
            val resultString = (SortedMap.empty[String, Polynomial] ++ result._1, result._2).toString
            resultString should equal (comment)
          }

        case _ =>
      }
      token = lexer.next()._1
    }


  }

}
