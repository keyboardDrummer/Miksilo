package transformations.bytecode

import application.compilerBuilder.PresetsPanel
import application.compilerCockpit.MarkOutputGrammar
import core.particles.{Particle, CompilerFromParticles}
import org.junit.{Assert, Test}
import transformations.bytecode.additions.{LabelledLocations, PoptimizeC}
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.JavaCompiler
import util.TestUtils

class TestLabelledLocations {

  val labelledParticles: Seq[Particle] = Seq(LabelledLocations) ++ JavaCompiler.byteCodeTransformations
  @Test
  def javaToLabelled() {
    val particles: Seq[Particle] = JavaCompiler.spliceBeforeTransformations(labelledParticles, Seq(MarkOutputGrammar))
    val utils = new TestUtils(new CompilerFromParticles(particles))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFile("Fibonacci.java"))
    val expectedResult = utils.getTestFile("FibonacciInLabelledByteCode.txt").slurp()
    Assert.assertEquals(expectedResult, result)
  }

  @Test
  def labelledToByteCode() {
    val labelledByteCodeCompiler = new CompilerFromParticles(labelledParticles)
    val utils = new TestUtils(new CompilerFromParticles(labelledByteCodeCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(utils.getTestFile("FibonacciInLabelledByteCode.txt"))
    val expectedResult = utils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    Assert.assertEquals(expectedResult, result)
  }
}
