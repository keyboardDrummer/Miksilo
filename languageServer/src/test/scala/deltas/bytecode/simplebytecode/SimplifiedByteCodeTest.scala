package deltas.bytecode.simplebytecode

import application.compilerBuilder.PresetsPanel
import org.scalatest.FunSuite
import util.{TestLanguageBuilder, SourceUtils, TestUtils}

class SimplifiedByteCodeTest extends FunSuite {

    test("javaToSimplified") {
    val utils = new TestUtils(TestLanguageBuilder.build(PresetsPanel.getJavaToSimplifiedByteCodePreset.deltas))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("simplifiedToByteCode") {
    val utils = new TestUtils(TestLanguageBuilder.build(PresetsPanel.getSimplifiedByteCodePreset.deltas))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
