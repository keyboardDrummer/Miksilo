package deltas.bytecode.simplebytecode

import core.deltas.Delta
import deltas.PrettyPrint
import deltas.javac.{ByteCodeLanguage, ExtendedByteCode, JavaToByteCodeLanguage}
import org.scalatest.FunSuite
import util.{LanguageTest, JavaSourceUtils, TestLanguageBuilder}

class SimplifiedByteCodeTest extends FunSuite {

  test("javaToSimplified") {
    val deltas = JavaToByteCodeLanguage.spliceBeforeTransformations(ExtendedByteCode.simpleByteCodeDeltas, Seq(PrettyPrint()))
    val utils = new LanguageTest(TestLanguageBuilder.build(deltas))
    val result = utils.compileAndPrettyPrint(JavaSourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = JavaSourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("simplifiedToByteCode") {
    val deltas = Delta.spliceAndFilterTop(ExtendedByteCode.simpleByteCodeDeltas, ByteCodeLanguage.byteCodeDeltas, Seq(PrettyPrint()))
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(deltas))
    val result = utils.compileAndPrettyPrint(JavaSourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = JavaSourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
