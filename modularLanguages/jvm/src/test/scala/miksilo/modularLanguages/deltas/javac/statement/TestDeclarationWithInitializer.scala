package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest

import scala.reflect.io.Path

class TestDeclarationWithInitializer extends JavaLanguageTest {

  test("test") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("DeclarationWithInitializer", inputDirectory)
  }
}
