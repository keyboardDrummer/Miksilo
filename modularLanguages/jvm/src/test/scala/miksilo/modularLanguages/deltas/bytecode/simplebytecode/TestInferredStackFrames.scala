package miksilo.modularLanguages.deltas.bytecode.simpleBytecode

import util.JavaLanguageTest

class TestInferredStackFrames extends JavaLanguageTest {

  test("regression1") {
    compareWithJavacAfterRunning("ComparisonOptimization.java")
  }
}
