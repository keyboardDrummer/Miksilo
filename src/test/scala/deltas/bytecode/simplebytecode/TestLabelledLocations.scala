package deltas.bytecode.simplebytecode

import application.compilerCockpit.MarkOutputGrammar
import core.deltas.Delta
import deltas.bytecode.additions.LabelledLocations
import deltas.bytecode.simpleBytecode.InlineConstantPool
import deltas.javac.JavaCompilerDeltas
import org.scalatest.FunSuite
import util.{CompilerBuilder, SourceUtils, TestUtils}

class TestLabelledLocations extends FunSuite {

  val labelledParticles: Seq[Delta] = Seq(LabelledLocations, InlineConstantPool) ++ JavaCompilerDeltas.byteCodeDeltas

  test("javaToLabelled") {
    val particles: Seq[Delta] = CompilerBuilder.build(JavaCompilerDeltas.javaCompilerDeltas).spliceBeforeTransformations(labelledParticles, Seq(MarkOutputGrammar))
    val utils = new TestUtils(CompilerBuilder.build(particles))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt")
      assertResult(expectedResult)(result)
  }

  test("labelledToByteCode") {
    val labelledByteCodeCompiler = CompilerBuilder.build(labelledParticles)
    val utils = new TestUtils(CompilerBuilder.build(labelledByteCodeCompiler.spliceBeforeTransformations(JavaCompilerDeltas.byteCodeDeltas, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
