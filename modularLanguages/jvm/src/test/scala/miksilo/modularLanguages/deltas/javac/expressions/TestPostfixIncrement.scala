package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest

import scala.reflect.io.Path

class TestPostfixIncrement extends JavaLanguageTest {

  test("basic") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("PostFixIncrement", inputDirectory)
  }
}
