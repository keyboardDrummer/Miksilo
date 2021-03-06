package miksilo.modularLanguages.deltas.bytecode.simpleBytecode

import miksilo.modularLanguages.core.deltas.Delta
import miksilo.modularLanguages.deltas.PrettyPrint
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage
import miksilo.modularLanguages.deltas.javac.{ExtendedByteCode, JavaToByteCodeLanguage}
import miksilo.editorParser.SourceUtils
import miksilo.modularLanguages.util.{JavaSourceUtils, LanguageTest, TestLanguageBuilder}
import org.scalatest.funsuite.AnyFunSuite

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
