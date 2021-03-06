package miksilo.modularLanguages.deltas.bytecode.simpleBytecode

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestInferredStackFrames extends AnyFunSuite {

  test("regression1") {
    JavaLanguageTest.compareWithJavacAfterRunning("ComparisonOptimization.java")
  }
}
