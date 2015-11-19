package transformations.javac

import application.compilerBuilder.PresetsPanel
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import util.TestUtils

import scala.reflect.io.File

class JavaToSimplifiedByteCode
  extends TestUtils(new CompilerFromParticles(PresetsPanel.getJavaToSimplifiedByteCodePreset.particles)) {

  @Test
  def comparePrintResult() {
    val resultFile: File = getJavaTestFile("WhileeWithComment.java")
    val expectedResult = resultFile.slurp()
    val result = compileAndPrettyPrint(getJavaTestFile("Fibonacci.java"))
    Assert.assertEquals(expectedResult, result)
  }
}
