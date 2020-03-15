package miksilo.modularLanguages.deltas.javac.statement

import util.JavaLanguageTest

import scala.reflect.io.Path

class TestExpressionAsStatement extends JavaLanguageTest {

  test("basic") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("ExpressionAsStatement", inputDirectory)
  }
}
