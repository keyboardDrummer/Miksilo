package transformations.bytecode

import application.compilerCockpit.MarkOutputGrammar
import core.particles.Delta
import org.scalatest.FunSuite
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.RemoveConstantPool
import transformations.javac.JavaCompiler
import util.{CompilerBuilder, SourceUtils}
import util.TestUtils

class TestLabelledLocations extends FunSuite {

  val labelledParticles: Seq[Delta] = Seq(LabelledLocations, RemoveConstantPool) ++ JavaCompiler.byteCodeTransformations

  test("javaToLabelled") {
    val particles: Seq[Delta] = CompilerBuilder.build(JavaCompiler.javaCompilerTransformations).spliceBeforeTransformations(labelledParticles, Seq(MarkOutputGrammar))
    val utils = new TestUtils(CompilerBuilder.build(particles))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt")
      assertResult(expectedResult)(result)
  }

  test("labelledToByteCode") {
    val labelledByteCodeCompiler = CompilerBuilder.build(labelledParticles)
    val utils = new TestUtils(CompilerBuilder.build(labelledByteCodeCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
