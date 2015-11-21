package transformations.bytecode

import org.junit.Test
import util.TestUtils

class TestInferredStackFrames {

  @Test
  def regression1(): Unit = {
    TestUtils.compareWithJavacAfterRunning("ComparisonOptimization.java")
  }
}
