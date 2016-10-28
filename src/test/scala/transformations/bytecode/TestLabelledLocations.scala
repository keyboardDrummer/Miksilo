package transformations.bytecode

import application.compilerBuilder.PresetsPanel
import application.compilerCockpit.MarkOutputGrammar
import core.particles.{CompilerFromParticles, Particle}
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import transformations.bytecode.additions.{LabelledLocations, PoptimizeC}
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.JavaCompiler
import util.TestUtils

class TestLabelledLocations extends FunSuite {

  val labelledParticles: Seq[Particle] = Seq(LabelledLocations) ++ JavaCompiler.byteCodeTransformations

  def javaToLabelled() {
    val particles: Seq[Particle] = JavaCompiler.spliceBeforeTransformations(labelledParticles, Seq(MarkOutputGrammar))
    val utils = new TestUtils(new CompilerFromParticles(particles))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFile("Fibonacci.java"))
    val expectedResult = utils.getTestFile("FibonacciInLabelledByteCode.txt").slurp()
    assertResult(expectedResult)(result)
  }


  def labelledToByteCode() {
    val labelledByteCodeCompiler = new CompilerFromParticles(labelledParticles)
    val utils = new TestUtils(new CompilerFromParticles(labelledByteCodeCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(utils.getTestFile("FibonacciInLabelledByteCode.txt"))
    val expectedResult = utils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    assertResult(expectedResult)(result)
  }
}
