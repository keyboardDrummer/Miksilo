package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import scala.reflect.io.Path

class TestExpressionAsStatement extends JavaLanguageTest {

  test("basic") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("ExpressionAsStatement", inputDirectory)
  }
}
