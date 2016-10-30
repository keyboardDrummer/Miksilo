package transformations.bytecode

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

class TestInferredStackFrames extends FunSuite {

  test("regression1") {
    TestUtils.compareWithJavacAfterRunning("ComparisonOptimization.java")
  }
}
