package transformations.javac.statement

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestExpressionAsStatement {

  @Test
  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("ExpressionAsStatement", inputDirectory)
  }
}
