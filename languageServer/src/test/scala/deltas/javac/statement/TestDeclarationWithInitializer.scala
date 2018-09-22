package deltas.javac.statement

import org.junit.Test
import org.scalatest.FunSuite
import util.LanguageTest

import scala.reflect.io.Path

class TestDeclarationWithInitializer extends FunSuite {

  test("test") {
    val inputDirectory = Path("")
    LanguageTest.compareWithJavacAfterRunning("DeclarationWithInitializer", inputDirectory)
  }
}
