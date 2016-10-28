package transformations.bytecode

import org.junit.Test
import util.TestUtils

class TestInferredStackFrames {


  def regression1(): Unit = {
    TestUtils.compareWithJavacAfterRunning("ComparisonOptimization.java")
  }
}
