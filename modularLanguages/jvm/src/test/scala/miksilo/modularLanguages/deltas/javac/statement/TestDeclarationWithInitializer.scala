package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class TestDeclarationWithInitializer extends AnyFunSuite {

  test("test") {
    val inputDirectory = Path("")
    JavaLanguageTest.compareWithJavacAfterRunning("DeclarationWithInitializer", inputDirectory)
  }
}
