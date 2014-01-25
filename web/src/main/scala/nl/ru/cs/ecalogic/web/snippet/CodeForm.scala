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

import scala.xml.{Unparsed, NodeSeq, Utility}
import net.liftweb.util.Helpers._
import net.liftweb.util.JsonCmd
import net.liftweb.http.SHtml.jsonForm
import net.liftweb.http.{LiftRules, SHtml, JsonHandler}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, SetHtml, Script}
import nl.ru.cs.ecalogic.parser.{ModelParser, Parser}
import nl.ru.cs.ecalogic.util.DefaultErrorHandler
import nl.ru.cs.ecalogic.analysis.{EnergyAnalysis, SemanticAnalysis}
import nl.ru.cs.ecalogic.{ECAException, config}
import java.io.{ByteArrayOutputStream, PrintWriter}
import nl.ru.cs.ecalogic.model.{ECMModel, ComponentModel}

object CodeForm {

  this.getClass.getResource("").toString

  def insertComponents = ("#code2 *" #> LiftRules.loadResourceAsString("/components/CPU.ecm").openOrThrowException("CPU Component not found")) &
    ("#code3 *" #> LiftRules.loadResourceAsString("/components/Radio.ecm").openOrThrowException("Radio Component not found")) &
    ("#code4 *" #> LiftRules.loadResourceAsString("/components/Sensor.ecm").openOrThrowException("Sensor Component not found"))

  def render =
    "#codeForm *" #> ((ns: NodeSeq) => jsonForm(AnalyseServer, insertComponents(ns))) &
      "#codeScript" #> Script(AnalyseServer.jsCmd) &
      "#add [onClick]" #> SHtml.onEvent((str) => Run("tabs=tabs+1;createNewTab('codeForm1','Component ' + (tabs - 1),'<textarea name=comp cols=80 rows=35; id=code' + tabs + '></textarea><br>','',true)"))


  object AnalyseServer extends JsonHandler {

    val errorStream = new ByteArrayOutputStream()
    val pw = new PrintWriter(errorStream)

    var components = Map.empty[String, ComponentModel]

    def processComponent(s: String) {
      val errorHandler = new DefaultErrorHandler(sourceText = Some(s), writer = pw)
      val loaded = ECMModel.fromSource(s, None, Some(errorHandler))
      if (new ModelParser(s, errorHandler).component().imports.nonEmpty)
        throw new ECAException(s"Import statement not allowed")
      components = components + (loaded.name -> loaded)
    }

    def apply(in: Any): JsCmd = in match {
      case JsonCmd("processForm", target, params: Map[String, _], all) =>
        errorStream.reset()

        val code = params.getOrElse("code", "").toString

        config.Options.reset
        if (params.getOrElse("tech", "") == "True") config.Options(Array("-tr"))
        if (params.getOrElse("beforeSync", "") == "True") config.Options(Array("-s"))
        if (params.getOrElse("update", "") == "True") config.Options(Array("-u"))

        val errorHandler = new DefaultErrorHandler(sourceText = Some(code), writer = pw)
        try {
          val parser = new Parser(code, errorHandler)
          val program = parser.program()
          parser.expectEndOfFile()
          if (program.imports.nonEmpty)
            throw new ECAException(s"Import statement not allowed")
          if (errorHandler.errorOccurred) {
            return SetHtml("result", Unparsed("Parse error: <pre><code>%s</pre></code>".format(xml.Utility.escape(errorStream.toString))))
          }

          components = Map.empty[String, ComponentModel]
          params.getOrElse("comp", None) match {
            case l: List[_] => l.foreach(s => processComponent(s.toString))
            case s: String => {
              processComponent(s)
            }
            case _ => {}
          }

          val checker = new SemanticAnalysis(program, components, errorHandler)
          checker.functionCallHygiene()
          checker.variableReferenceHygiene()
          if (errorHandler.errorOccurred) {
            return SetHtml("result", Unparsed("Semantic error: <pre><code>%s</pre></code>".format(Utility.escape(errorStream.toString))))
          }

          val consumptionAnalyser = new EnergyAnalysis(program, components, errorHandler)

          if (errorHandler.errorOccurred) {
            return SetHtml("result", Unparsed("Analyse error: <pre><code>%s</pre></code>".format(Utility.escape(errorStream.toString))))
          }

          val state = consumptionAnalyser.analyse()
          val buf = new StringBuilder
          state.transform((_, st) => st.energy) match {
            case (states, t) =>
              buf append f"Time:\t$t%s<br>"
              buf append f"Energy:\t${states.values.reduce(_ + _)}%s<br>"
              for ((name, e) <- states)
                buf append f"└ ${xml.Utility.escape(name)}%13s\t$e%s<br>"
          }

          SetHtml("result", Unparsed("The result is:<br>%s".format(buf.toString)))
        } catch {
          case e: nl.ru.cs.ecalogic.ECAException =>
            return SetHtml("result", Unparsed(s"Fatal error: <pre><code>${Utility.escape(errorStream.toString)} ${e.getMessage}</code></pre>"))
        }
    }
  }

}
