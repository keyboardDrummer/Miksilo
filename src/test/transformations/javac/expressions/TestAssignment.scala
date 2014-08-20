package transformations.javac.expressions

import org.junit.{Assert, Test}
import util.TestUtils

import scala.reflect.io.Path

class TestAssignment {

  @Test
  def testFullPipeline() {
    val inputDirectory = Path("")
    val output: String = TestUtils.compileAndRun("Assignment", inputDirectory)
    Assert.assertEquals("1", output)
  }


  @Test
  def testAssignmentWithJump() {
    val inputDirectory = Path("")
    val output: String = TestUtils.compileAndRun("AssignmentWithJump", inputDirectory)
    Assert.assertEquals("1", output)
  }
}
