package deltas.javac.statement

import util.JavaLanguageTest

import scala.reflect.io.Path

class TestDeclarationWithInitializer extends JavaLanguageTest {

  test("test") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("DeclarationWithInitializer", inputDirectory)
  }
}
