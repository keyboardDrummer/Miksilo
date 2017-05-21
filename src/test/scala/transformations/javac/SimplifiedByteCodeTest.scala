package transformations.javac

import application.compilerBuilder.PresetsPanel
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import util.TestUtils

class SimplifiedByteCodeTest extends FunSuite {

  test("javaToSimplified") {
    val utils = new TestUtils(new CompilerFromParticles(PresetsPanel.getJavaToSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = utils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("simplifiedToByteCode") {
    val utils = new TestUtils(new CompilerFromParticles(PresetsPanel.getSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(utils.getTestFileContents("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = utils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
