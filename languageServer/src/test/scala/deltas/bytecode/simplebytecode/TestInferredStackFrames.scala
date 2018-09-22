package deltas.bytecode.simplebytecode

import org.scalatest.FunSuite
import util.LanguageTest

class TestInferredStackFrames extends FunSuite {

  test("regression1") {
    LanguageTest.compareWithJavacAfterRunning("ComparisonOptimization.java")
  }
}
