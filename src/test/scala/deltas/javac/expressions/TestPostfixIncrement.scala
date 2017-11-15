package deltas.javac.expressions

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

import scala.reflect.io.Path

class TestPostfixIncrement extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("postFixIncrement", inputDirectory)
  }
}
