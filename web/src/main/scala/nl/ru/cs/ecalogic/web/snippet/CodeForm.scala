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
import nl.ru.cs.ecalogic.analysis.{EnergyAnalysis, SemanticAnalysis}
import nl.ru.cs.ecalogic.{ECAException, config}
import java.io.{File, ByteArrayOutputStream, PrintWriter}
import scala.io.Source
import nl.ru.cs.ecalogic.model.{ECMModel, ComponentModel}

object CodeForm {


  def insertComponents = ("#code2 *" #> Source.fromFile(new File("components/ecalogic/CPU.ecm")).mkString) &
    ("#code3 *" #> Source.fromFile(new File("components/ecalogic/Radio.ecm")).mkString) &
    ("#code4 *" #> Source.fromFile(new File("components/ecalogic/Sensor.ecm")).mkString) &
    ("#code5 *" #> Source.fromFile(new File("components/ecalogic/Stub.ecm")).mkString)

  def render =
    "#codeForm" #> ((ns: NodeSeq) => jsonForm(AnalyseServer, insertComponents(ns))) &
      "#codeScript" #> Script(AnalyseServer.jsCmd)

  object AnalyseServer extends JsonHandler {

    val errorStream = new ByteArrayOutputStream()
    val pw = new PrintWriter(errorStream)

    var components = Map.empty[String, ComponentModel]

    def processComponent(s: String) {
      val errorHandler = new DefaultErrorHandler(sourceText = Some(s), writer = pw)
      val loaded = ECMModel.fromSource(s, None, errorHandler)
      components = components + (loaded.name -> loaded)
    }

    def apply(in: Any): JsCmd = in match {
      case JsonCmd("processForm", target, params: Map[String, _], all) =>
        val code: String = params.getOrElse("code", "").toString()


        config.Options.reset
        if (params.getOrElse("tech", "") == "True") config.Options(Array("-tr"))
        if (params.getOrElse("beforeSync", "") == "True") config.Options(Array("-s"))
        if (params.getOrElse("update", "") == "True") config.Options(Array("-u"))

        val errorHandler = new DefaultErrorHandler(sourceText = Some(code), writer = pw)
        try {
          val parser = new Parser(code, errorHandler)
          val program = parser.program()
          if (errorHandler.errorOccurred) {
            return SetHtml("result", scala.xml.Unparsed("Parse error: <pre><code>%s</pre></code>".format(xml.Utility.escape(errorStream.toString))))
          }

          components = Map.empty[String, ComponentModel]
          params.getOrElse("comp", None) match {
            case l: List[_] => l.foreach(s => processComponent(s.toString))
            case s: String => {
              processComponent(s)
            }
            case _ => {}
          }

          //val components = Map("Stub" -> StubComponent, "BAD" -> BadComponent, "Sensor" -> Sensor, "Radio" -> Radio) ++ (if (params.getOrElse("CPU", "") == "True") Map.empty else Map("CPU" -> CPU))
          val checker = new SemanticAnalysis(program, components, errorHandler)
          checker.functionCallHygiene()
          checker.variableReferenceHygiene()
          if (errorHandler.errorOccurred) {
            return SetHtml("result", scala.xml.Unparsed("Semantic error: <pre><code>%s</pre></code>".format(xml.Utility.escape(errorStream.toString))))
          }

          val consumptionAnalyser = new EnergyAnalysis(program, components, errorHandler)

          if (errorHandler.errorOccurred) {
            return SetHtml("result", scala.xml.Unparsed("Analyse error: <pre><code>%s</pre></code>".format(xml.Utility.escape(errorStream.toString))))
          }
          SetHtml("result", scala.xml.Unparsed("The result is %s".format(xml.Utility.escape(consumptionAnalyser.analyse().toString))))
        } catch {
          case e: nl.ru.cs.ecalogic.ECAException =>
            return SetHtml("result", scala.xml.Unparsed("Fatal error: <pre><code>%s</pre></code>".format(xml.Utility.escape(errorStream.toString))))
        }
    }
  }

}
