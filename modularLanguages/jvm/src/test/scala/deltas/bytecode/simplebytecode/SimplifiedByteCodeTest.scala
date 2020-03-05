package deltas.bytecode.simpleBytecode

import core.SourceUtils
import core.deltas.Delta
import deltas.PrettyPrint
import deltas.bytecode.ByteCodeLanguage
import deltas.javac.{ExtendedByteCode, JavaToByteCodeLanguage}
import org.scalatest.funsuite.AnyFunSuite
import util.{JavaSourceUtils, LanguageTest, TestLanguageBuilder}

class SimplifiedByteCodeTest extends AnyFunSuite {

  test("javaToSimplified") {
    val deltas = JavaToByteCodeLanguage.spliceBeforeTransformations(ExtendedByteCode.simpleByteCodeDeltas, Seq(PrettyPrint()))
    val utils = new LanguageTest(TestLanguageBuilder.build(deltas))
    val result = utils.compileAndPrettyPrint(JavaSourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getResourceFileContents("FibonacciInSimplifiedByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("simplifiedToByteCode") {
    val deltas = Delta.spliceAndFilterTop(ExtendedByteCode.simpleByteCodeDeltas, ByteCodeLanguage.byteCodeDeltas, Seq(PrettyPrint()))
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(deltas))
    val result = utils.compileAndPrettyPrint(SourceUtils.getResourceFileContents("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = SourceUtils.getResourceFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
