package transformations.javac

import org.junit.{Assert, Test}

import scala.reflect.io.Path

class TestAssignment {

  @Test
  def testFullPipeline() {
    val inputDirectory = Path("remy")
    val output: String = TestUtils.compileAndRun("Assignment", inputDirectory)
    Assert.assertEquals("1", output)
  }
}
