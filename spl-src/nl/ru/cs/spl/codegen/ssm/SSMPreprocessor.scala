package nl.ru.cs.spl.codegen.ssm

import scala.io.Source
import org.apache.commons.lang3.StringEscapeUtils
import java.io.File

class SSMPreprocessor(annotate: Boolean = false, referenceCounting: Boolean = false) {

  def preprocess(input: String): String =
    Seq(
      (s: String) => "(?m)\\s*(?:(?:;|(?://)).*)?$".r.replaceAllIn(s, ""),
      (s: String) => "(?m)^(\\s*)#((?:release)|(?:addref)|(?:addref_right)|(?:addref_left))\\s+([-]?\\d+)\\s*\n".r.replaceAllIn(s, m =>
        if (referenceCounting) s"${m.group(1)}lds ${m.group(3)}\n${m.group(1)}bsr __${m.group(2)}\n" else ""
      ),
      (s: String) => "(?m)^(\\s*)#print\\s+\"((?:[^\"\\\\]|\\\\.)*)\"\\s*\n".r.replaceAllIn(s, m =>
        StringEscapeUtils.unescapeJava(m.group(2)).map(c => s"${m.group(1)}ldc ${c.toInt}\n${m.group(1)}trap 1\n").mkString
      ),
      (s: String) => "(?m)^\\s*#include\\s+\"([^\"]*)\"\\s*$".r.replaceAllIn(s, m =>
        preprocess(Source.fromFile(new File("include", m.group(1))).mkString + "\n")
      )
    ).foldLeft(input)((s, f) => f(s))


  def main(args: Array[String]) {
    print(preprocess(Source.fromFile("test").mkString))
  }

}
