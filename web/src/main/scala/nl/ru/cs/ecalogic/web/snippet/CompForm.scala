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

import java.io.{ByteArrayOutputStream, PrintWriter, FileNotFoundException, File}
import scala.io.Source
import _root_.net.liftweb.http._
import js._
import JsCmds._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._
import net.liftweb.http.SHtml._
import net.liftweb.util.JsonCmd
import net.liftweb.http.js.JsCmds.SetHtml
import java.lang.NumberFormatException
import nl.ru.cs.ecalogic.util.DefaultErrorHandler
import nl.ru.cs.ecalogic.model.ECMModel

object CompForm {

  def componentsList = {
    val sb = new StringBuilder
    var i = -1;
    sb.append("<select name=\"load\">\n")
    new File("components/ecalogic/").listFiles().foreach({
      f => sb append (
        "<option value=\"" + {
          i += 1;
          i
        } /* scala does not support i++, what?!? */ + "\">" + f.getName + "</option>\n")
    })
    sb.append("<option value=\"new\">New component</option>\n>")
    sb.append("</select>\n            <input type=\"submit\" value=\"Load\"/>")
    scala.xml.Unparsed(sb.toString())
  }

  def render =
    "#compForm" #> ((ns: NodeSeq) => jsonForm(AnalyseServer, componentsList)) &
      "#compScript" #> Script(AnalyseServer.jsCmd)

  var newTabs = 0;

  object AnalyseServer extends JsonHandler {
    def apply(in: Any): JsCmd = in match {
      case JsonCmd("processForm", target, params: Map[String, String], all) =>
        val load = params.getOrElse("load", "")
        newTabs = newTabs + 1
        if (load == "new") {
          return SHtml.onEvent((str) => Run(s"createNewTab('codeForm1','Component${newTabs}','<textarea name=comp cols=80 rows=35; id=code${newTabs}></textarea><br>','',true)"));
        }
        try {
          val file = new File("components/ecalogic/").listFiles()(load.toInt)

          if (file.isDirectory) {
            // TODO: ?Walk directory?
            return SetHtml("result", scala.xml.Unparsed("Not a file."))
          }

          val source = Source.fromFile(file).mkString

          val errorStream = new ByteArrayOutputStream()
          val pw = new PrintWriter(errorStream)
          val errorHandler = new DefaultErrorHandler(sourceText = Some(source), writer = pw)
          val loaded = ECMModel.fromSource(source, None, errorHandler)
          if (errorHandler.errorOccurred) {
            return SetHtml("result", scala.xml.Unparsed("Parse error: <pre><code>%s</pre></code>".format(xml.Utility.escape(errorStream.toString))))
          }


          val name = loaded.name+ (newTabs - 1)
          val qname = "\"" + name + "\"" // More fun with scala, escape quotes is not support in string interpolation, see SI-6476
          return SHtml.onEvent((str) => Run(s"createNewTab('codeForm1','${name}','<textarea name=comp cols=80 rows=35; id=${qname}>${source.replaceAll("\r*\n\r*", "' + String.fromCharCode(13) + '")}</textarea><br>','',true)"))
        } catch {
          case e @ (_ : NumberFormatException | _ : FileNotFoundException | _ : ArrayIndexOutOfBoundsException) =>
            return SetHtml("result", scala.xml.Unparsed("%s: %s".format(xml.Utility.escape(e.toString))))
        }
    }
  }

}
