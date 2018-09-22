package deltas.bytecode.simplebytecode

import core.deltas.Delta
import deltas.PrettyPrint
import deltas.javac.JavaLanguage
import org.scalatest.FunSuite
import util.{SourceUtils, TestLanguageBuilder, LanguageTest}

class SimplifiedByteCodeTest extends FunSuite {

  test("javaToSimplified") {
    val deltas = JavaLanguage.spliceBeforeTransformations(JavaLanguage.simpleByteCodeDeltas, Seq(PrettyPrint()))
    val utils = new LanguageTest(TestLanguageBuilder.build(deltas))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("simplifiedToByteCode") {
    val deltas = Delta.spliceAndFilterTop(JavaLanguage.simpleByteCodeDeltas, JavaLanguage.byteCodeDeltas, Seq(PrettyPrint()))
    val utils = new LanguageTest(TestLanguageBuilder.build(deltas))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }
}
