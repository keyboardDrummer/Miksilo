package deltas.javac.statement

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

import scala.reflect.io.Path

class TestExpressionAsStatement extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("ExpressionAsStatement", inputDirectory)
  }
}
