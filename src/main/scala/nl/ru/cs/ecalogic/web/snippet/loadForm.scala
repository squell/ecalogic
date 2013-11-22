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
import java.io.{FileNotFoundException, File, ByteArrayOutputStream, PrintWriter}
import net.liftmodules.textile._
import scala.io.Source

object LoadForm {

  def render =
    "#loadForm" #> ((ns:NodeSeq) => jsonForm(AnalyseServer, ns)) &
      "#loadScript" #> Script(AnalyseServer.jsCmd)

  object AnalyseServer extends JsonHandler {
      def apply(in: Any): JsCmd = in match {
      case JsonCmd("processForm", target, params: Map[String, String], all) =>
        val load = params.getOrElse("load", "")

      // TODO: Find file
      try {
        val file = new File("/doc/examples/" + load)

        val source = Source.fromFile(file).mkString

        SetHtml("result2", Text(source))
      } catch {
        case e: FileNotFoundException =>
          return SetHtml("result", scala.xml.Unparsed("File not found: %s".format(e.getMessage)))
      }
    }
    //def apply(in: Any): JsCmd = SetHtml("code", Text("Bla"))
  }

}