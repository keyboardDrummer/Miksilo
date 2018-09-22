package deltas.javac.statement

import org.junit.Test
import org.scalatest.FunSuite
import util.LanguageTest

import scala.reflect.io.Path

class TestExpressionAsStatement extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    LanguageTest.compareWithJavacAfterRunning("ExpressionAsStatement", inputDirectory)
  }
}
