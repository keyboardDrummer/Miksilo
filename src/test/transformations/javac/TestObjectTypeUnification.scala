package transformations.javac

import org.junit.{Assert, Test}

import scala.reflect.io.Path

class TestObjectTypeUnification {

  @Test
  def testFullPipeline() {
    val inputDirectory = Path("")
    val output: String = TestUtils.compileAndRun("ObjectTypeUnification", inputDirectory)
    Assert.assertEquals("3", output)
  }

  @Test
  def compareWithJavaC() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("ObjectTypeUnification", inputDirectory)
  }
}
