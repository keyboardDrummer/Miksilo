package transformations.javac.expressions

import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import util.TestUtils

import scala.reflect.io.Path

class TestAssignment extends FunSuite {

  def testFullPipeline() {
    val inputDirectory = Path("")
    val output: String = TestUtils.compileAndRun("Assignment", inputDirectory)
    assertResult("1")(output)
  }

  def testAssignmentWithJump() {
    val inputDirectory = Path("")
    val output: String = TestUtils.compileAndRun("AssignmentWithJump", inputDirectory)
    assertResult("1")(output)
  }
}
