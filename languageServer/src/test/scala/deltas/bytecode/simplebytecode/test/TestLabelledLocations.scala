package deltas.bytecode.simplebytecode.test

import application.compilerCockpit.MarkOutputGrammar
import core.deltas.Delta
import deltas.bytecode.simpleBytecode.{InlineConstantPool, LabelledLocations}
import deltas.javac.JavaLanguage
import org.scalatest.FunSuite
import util.{TestLanguageBuilder, SourceUtils, TestUtils}

class TestLabelledLocations extends FunSuite {

  val labelledParticles: Seq[Delta] = Seq(LabelledLocations, InlineConstantPool) ++ JavaLanguage.byteCodeDeltas

  test("javaToLabelled") {
    val particles: Seq[Delta] = TestLanguageBuilder.build(JavaLanguage.javaCompilerDeltas).spliceBeforeTransformations(labelledParticles, Seq(MarkOutputGrammar))
    val utils = new TestUtils(TestLanguageBuilder.build(particles))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("Fibonacci.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt")
    assertResult(expectedResult)(result)
  }

  test("labelledToByteCode") {
    val labelledByteCodeCompiler = TestLanguageBuilder.build(labelledParticles)
    val utils = new TestUtils(TestLanguageBuilder.build(labelledByteCodeCompiler.spliceBeforeTransformations(JavaLanguage.byteCodeDeltas, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    assertResult(expectedResult)(result)
  }

  test("labelledToInlinedByteCode") {
    val labelledByteCodeCompiler = TestLanguageBuilder.build(labelledParticles)
    val utils = new TestUtils(TestLanguageBuilder.build(labelledByteCodeCompiler.spliceBeforeTransformations(Seq(InlineConstantPool) ++ JavaLanguage.byteCodeDeltas, Seq(MarkOutputGrammar))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getTestFileContents("FibonacciInLabelledByteCode.txt"))
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
