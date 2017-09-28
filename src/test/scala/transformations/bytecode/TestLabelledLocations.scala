package transformations.bytecode

import application.compilerCockpit.MarkOutputGrammar
import core.particles.Delta
import org.scalatest.FunSuite
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.simpleBytecode.RemoveConstantPool
import transformations.javac.JavaCompiler
import util.{CompilerBuilder, TestUtils}

class TestLabelledLocations extends FunSuite {

  val labelledParticles: Seq[Delta] = Seq(LabelledLocations, RemoveConstantPool) ++ JavaCompiler.byteCodeTransformations

  test("javaToLabelled") {
    val particles: Seq[Delta] = CompilerBuilder.build(JavaCompiler.javaCompilerTransformations).spliceBeforeTransformations(labelledParticles, Seq(MarkOutputGrammar))
    val utils = new TestUtils(CompilerBuilder.build(particles))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = utils.getTestFileContents("FibonacciInLabelledByteCode.txt")
      assertResult(expectedResult)(result)
  }

  test("labelledToByteCode") {
    val labelledByteCodeCompiler = CompilerBuilder.build(labelledParticles)
    val utils = new TestUtils(CompilerBuilder.build(labelledByteCodeCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(utils.getTestFileContents("FibonacciInLabelledByteCode.txt"))
    val expectedResult = utils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
