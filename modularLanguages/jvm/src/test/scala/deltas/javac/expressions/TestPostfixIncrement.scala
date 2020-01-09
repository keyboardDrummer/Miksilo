package deltas.javac.expressions

import util.JavaLanguageTest

import scala.reflect.io.Path

class TestPostfixIncrement extends JavaLanguageTest {

  test("basic") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("PostFixIncrement", inputDirectory)
  }
}
