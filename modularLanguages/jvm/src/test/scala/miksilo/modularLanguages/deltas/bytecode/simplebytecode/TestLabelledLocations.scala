package miksilo.modularLanguages.deltas.bytecode.simpleBytecode

import miksilo.modularLanguages.core.deltas.Delta
import miksilo.modularLanguages.deltas.PrettyPrint
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{InlineConstantPool, LabelledLocations}
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import miksilo.editorParser.SourceUtils
import miksilo.modularLanguages.util.{JavaSourceUtils, TestLanguageBuilder}
import miksilo.modularLanguagesutil.LanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestLabelledLocations extends AnyFunSuite {

  val labelledParticles: Seq[Delta] = Seq(LabelledLocations, InlineConstantPool) ++ ByteCodeLanguage.byteCodeDeltas

  test("javaToLabelled") {
    val particles: Seq[Delta] = TestLanguageBuilder.build(JavaToByteCodeLanguage.javaCompilerDeltas).spliceBeforeTransformations(labelledParticles, Seq(PrettyPrint()))
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(particles))
    val result = utils.compileAndPrettyPrint(JavaSourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getResourceFileContents("FibonacciInLabelledByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("labelledToByteCode") {
    val labelledByteCodeCompiler = TestLanguageBuilder.build(labelledParticles)
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(labelledByteCodeCompiler.spliceBeforeTransformations(ByteCodeLanguage.byteCodeDeltas, Seq(PrettyPrint()))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getResourceFileContents("FibonacciInLabelledByteCode.txt"))
    val expectedResult = SourceUtils.getResourceFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }

  test("labelledToInlinedByteCode") {
    val labelledByteCodeCompiler = TestLanguageBuilder.build(labelledParticles)
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(labelledByteCodeCompiler.spliceBeforeTransformations(Seq(InlineConstantPool) ++ ByteCodeLanguage.byteCodeDeltas, Seq(PrettyPrint()))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getResourceFileContents("FibonacciInLabelledByteCode.txt"))
    val expectedResult =
      """class Fibonacci extends java/lang/Object with: ()
        |{
        |  Method;
        |    name: <init>
        |    descriptor: ()V
        |    flags: ACC_PUBLIC
        |    Code:
        |      name: Code, stack:1, locals:1
        |        aload 0
        |        invokespecial java/lang/Object.<init> ()V
        |        return
        |      Exceptions:
        |
        |  Method;
        |    name: main
        |    descriptor: ([Ljava/lang/String;)V
        |    flags: ACC_STATIC, ACC_PUBLIC
        |    Code:
        |      name: Code, stack:2, locals:1
        |        getstatic java/lang/System.out Ljava/io/PrintStream;
        |        iconst 5
        |        invokestatic Fibonacci.fibonacci (I)I
        |        invokevirtual java/io/PrintStream.print (I)V
        |        return
        |      Exceptions:
        |
        |  Method;
        |    name: fibonacci
        |    descriptor: (I)I
        |    flags: ACC_STATIC, ACC_PUBLIC
        |    Code:
        |      name: Code, stack:3, locals:1
        |        iload 0
        |        iconst 2
        |        if_icmpge 7
        |        iconst 1
        |        goto 16
        |        iload 0
        |        iconst 1
        |        isub
        |        invokestatic Fibonacci.fibonacci (I)I
        |        iload 0
        |        iconst 2
        |        isub
        |        invokestatic Fibonacci.fibonacci (I)I
        |        iadd
        |        ireturn
        |      StackMapTable: name: StackMapTable
        |        sameFrame, offset:9
        |        sameLocalsOneStackItem, offset:12
        |          int
        |      Exceptions:
        |}""".stripMargin
    assertResult(expectedResult)(result)
  }
}
