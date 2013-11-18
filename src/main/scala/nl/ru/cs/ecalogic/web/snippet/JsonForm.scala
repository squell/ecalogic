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

package nl.ru.cs.ecalogic.web.snippet

import scala.xml.{Text, NodeSeq}

import net.liftweb.util.Helpers._
import net.liftweb.util.JsonCmd
import net.liftweb.http.SHtml.jsonForm
import net.liftweb.http.JsonHandler
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{SetHtml, Script}
import nl.ru.cs.ecalogic.parser.Parser
import nl.ru.cs.ecalogic.util.DefaultErrorHandler
import nl.ru.cs.ecalogic.model.examples.{BadComponent, StubComponent}
import nl.ru.cs.ecalogic.model.examples.DemoComponents.{CPU, Radio, Sensor}
import nl.ru.cs.ecalogic.analysis.{EnergyAnalysis, SemanticAnalysis}
import nl.ru.cs.ecalogic.config
import java.io.ByteArrayOutputStream
import java.io.PrintWriter

object JsonForm {

  def render =
    "#jsonForm" #> ((ns: NodeSeq) => jsonForm(AnalyseServer, ns)) &
      "#jsonScript" #> Script(AnalyseServer.jsCmd)

  object AnalyseServer extends JsonHandler {
    def apply(in: Any): JsCmd = in match {
      case JsonCmd("processForm", target, params: Map[String, String], all) =>
        try {
          val code = params.getOrElse("code", "")

          val baos = new ByteArrayOutputStream()
          val pw = new PrintWriter(baos)
          val errorHandler = new DefaultErrorHandler(source = Some(code), writer = pw)
          val parser = new Parser(code, errorHandler)
          val program = parser.program()
          if (errorHandler.errorOccurred) {
            // TODO is this safe?
            return SetHtml("result", scala.xml.Unparsed("Parse error: %s".format(baos.toString).replace("\n", "<br>")))
          }

          val checker = new SemanticAnalysis(program, errorHandler)
          checker.functionCallHygiene()
          checker.variableReferenceHygiene()
          if (errorHandler.errorOccurred) {
            return SetHtml("result", scala.xml.Unparsed("Semantic error: %s".format(baos.toString)))
          }

          val components = Set(StubComponent, BadComponent, Sensor, Radio, if (config.Options.noCPU) StubComponent else CPU)

          val consumptionAnalyser = new EnergyAnalysis(program, components, errorHandler)

          if (errorHandler.errorOccurred) {
            // TODO do we need this?
            return SetHtml("result", scala.xml.Unparsed("Analyse error: %s".format(baos.toString)))
          }


          SetHtml("result", scala.xml.Unparsed("The code is %s".format(consumptionAnalyser().toString)))
        } catch {
          case e: nl.ru.cs.ecalogic.ECAException =>
            return SetHtml("result", scala.xml.Unparsed("Fatal analyse error: %s".format(e.getMessage)))
        }
    }
  }

}