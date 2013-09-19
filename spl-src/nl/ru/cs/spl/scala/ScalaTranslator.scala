package nl.ru.cs.spl.scala

import scala.io.Source
import scala.tools.nsc.{Settings, Global}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.ConsoleReporter

import java.io.{StringWriter, PrintWriter, File}

import nl.ru.cs.spl.parser.Parser

object ScalaTranslator {

  def main(args: Array[String]) {
    val arg = if (args.length == 0) None else Some(args(0))
    val file = new File(arg getOrElse "Example0_Factorial.spl").getCanonicalFile
    val source = Source.fromFile(file)
    val content = source.mkString
    source.close()
    val program = new Parser(content) program()

    val writer = new StringWriter()
    val printer = new ScalaPrinter(new PrintWriter(writer))
    val fileNameNoExt = file.getName.takeWhile(_ != '.')

    printer.printObject(fileNameNoExt, program)
    printer.flush()
    val scalaSource = new BatchSourceFile(fileNameNoExt + ".scala", writer.toString)

    val settings = new Settings()
    settings.outputDirs.setSingleOutput("gen")
    settings.usejavacp.value = true

    val reporter = new ConsoleReporter(settings)
    val compiler = new Global(settings, reporter)
    val run = new compiler.Run()
    run.compileSources(List(scalaSource))

    if (compiler.reporter.hasErrors) {
      sys.exit(1)
    }
  }

}