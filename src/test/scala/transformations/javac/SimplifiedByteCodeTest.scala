package transformations.javac

import application.compilerBuilder.PresetsPanel
import org.scalatest.FunSuite
import util.{CompilerBuilder, TestUtils}

class SimplifiedByteCodeTest extends FunSuite {

  test("javaToSimplified") {
    val utils = new TestUtils(CompilerBuilder.build(PresetsPanel.getJavaToSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = utils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("simplifiedToByteCode") {
    val utils = new TestUtils(CompilerBuilder.build(PresetsPanel.getSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(utils.getTestFileContents("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = utils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
