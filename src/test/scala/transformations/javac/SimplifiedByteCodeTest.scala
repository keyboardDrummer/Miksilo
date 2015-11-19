package transformations.javac

import application.compilerBuilder.PresetsPanel
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import util.TestUtils

import scala.reflect.io.File

class SimplifiedByteCodeTest  {

  @Test
  def javaToSimplified() {
    val utils = new TestUtils(new CompilerFromParticles(PresetsPanel.getJavaToSimplifiedByteCodePreset.particles))
    val resultFile: File = utils.getTestFile("FibonacciInSimplifiedByteCode.txt")
    val expectedResult = resultFile.slurp()
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFile("Fibonacci.java"))
    Assert.assertEquals(expectedResult, result)
  }

  @Test
  def simplifiedToByteCode() {
    val utils = new TestUtils(new CompilerFromParticles(PresetsPanel.getSimplifiedByteCodePreset.particles))
    val resultFile: File = utils.getTestFile("FibonacciByteCodePrettyPrinted.txt")
    val expectedResult = resultFile.slurp()
    val result = utils.compileAndPrettyPrint(utils.getTestFile("FibonacciInSimplifiedByteCode.txt"))
    Assert.assertEquals(expectedResult, result)
  }
}
