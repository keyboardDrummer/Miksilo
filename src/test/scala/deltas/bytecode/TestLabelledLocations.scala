package deltas.bytecode

import application.compilerCockpit.MarkOutputGrammar
import core.deltas.Delta
import org.scalatest.FunSuite
import deltas.bytecode.additions.LabelledLocations
import deltas.bytecode.simpleBytecode.RemoveConstantPool
import deltas.javac.JavaCompilerDeltas
import util.{CompilerBuilder, SourceUtils}
import util.TestUtils

class TestLabelledLocations extends FunSuite {

  val labelledParticles: Seq[Delta] = Seq(LabelledLocations, RemoveConstantPool) ++ JavaCompilerDeltas.byteCodeTransformations

  test("javaToLabelled") {
    val particles: Seq[Delta] = CompilerBuilder.build(JavaCompilerDeltas.javaCompilerDeltas).spliceBeforeTransformations(labelledParticles, Seq(MarkOutputGrammar))
    val utils = new TestUtils(CompilerBuilder.build(particles))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt")
      assertResult(expectedResult)(result)
  }

  test("labelledToByteCode") {
    val labelledByteCodeCompiler = CompilerBuilder.build(labelledParticles)
    val utils = new TestUtils(CompilerBuilder.build(labelledByteCodeCompiler.spliceBeforeTransformations(JavaCompilerDeltas.byteCodeTransformations, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
