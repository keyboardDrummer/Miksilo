package deltas.javac.expressions

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

import scala.reflect.io.Path

class TestIncrementAssignment extends FunSuite {

  test("incrementAssignment") {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("IncrementAssignment", inputDirectory)
  }
}
