package transformations.javac

import application.compilerBuilder.PresetsPanel
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import util.TestUtils

class SimplifiedByteCodeTest  {

  @Test
  def javaToSimplified() {
    val utils = new TestUtils(new CompilerFromParticles(PresetsPanel.getJavaToSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFile("Fibonacci.java"))
    val expectedResult = utils.getTestFile("FibonacciInSimplifiedByteCode.txt").slurp()
    Assert.assertEquals(expectedResult, result)
  }

  @Test
  def simplifiedToByteCode() {
    val utils = new TestUtils(new CompilerFromParticles(PresetsPanel.getSimplifiedByteCodePreset.particles))
    val result = utils.compileAndPrettyPrint(utils.getTestFile("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = utils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    Assert.assertEquals(expectedResult, result)
  }
}
