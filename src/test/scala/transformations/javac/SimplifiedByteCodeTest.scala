package transformations.javac

import application.compilerBuilder.PresetsPanel
import org.scalatest.FunSuite
import util.{CompilerBuilder, TestUtils, SourceUtils}

class SimplifiedByteCodeTest extends FunSuite {

    test("javaToSimplified") {
    val utils = new TestUtils(CompilerBuilder.build(PresetsPanel.getJavaToSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("simplifiedToByteCode") {
    val utils = new TestUtils(CompilerBuilder.build(PresetsPanel.getSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
