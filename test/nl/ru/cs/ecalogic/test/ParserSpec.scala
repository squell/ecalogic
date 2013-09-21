package nl.ru.cs.ecalogic.test

import org.scalatest.FlatSpec
import nl.ru.cs.ecalogic.parser.Parser
import scala.io.Source
import nl.ru.cs.ecalogic.util.DefaultErrorHandler
import java.io.File

// Kleine test class, maar ik snap nog niet heel veel van ScalaTest
class ParserSpec extends FlatSpec {

  behavior of "The Parser"

  it should "succeed on the test files" in {
    new File("zooi").listFiles().withFilter(_.getName.endsWith(".eca")).foreach { f =>
      val source = Source.fromFile(f).mkString
      val parser = new Parser(source, new DefaultErrorHandler(source = Some(source)))
      parser.program()
    }
  }

}
