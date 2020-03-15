package miksilo.modularLanguages.deltas.bytecode.simpleBytecode

import miksilo.modularLanguages.core.bigrammar.TestLanguageGrammarUtils
import miksilo.modularLanguages.deltas.PrettyPrint
import miksilo.modularLanguages.deltas.bytecode.{ByteCodeLanguage, ConstantPoolIndices}
import miksilo.modularLanguages.util.{LanguageTest, TestLanguageBuilder}
import org.scalatest.funsuite.AnyFunSuite

class InlineConstantPoolTest extends AnyFunSuite {

  private val inlinedConstantPoolFibonacci =
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
      |        same frame, offset:9
      |        same locals, 1 stack item, offset:12
      |          int
      |      Exceptions:
      |}
    """.stripMargin

  private val bytecodeFibonacci =
    """class 4 extends 26 with: ()
      |ConstantPool:
      |  Utf8 Fibonacci
      |  Utf8 fibonacci
      |  Utf8 (I)I
      |  Class 1
      |  NameAndType 2 3
      |  Methodref 4.5
      |  Utf8 StackMapTable
      |  Utf8 Code
      |  Utf8 java/io/PrintStream
      |  Utf8 print
      |  Utf8 (I)V
      |  Class 9
      |  NameAndType 10 11
      |  Methodref 12.13
      |  Utf8 out
      |  Utf8 Ljava/io/PrintStream;
      |  Utf8 java/lang/System
      |  Class 17
      |  NameAndType 15 16
      |  Fieldref 18.19
      |  Utf8 main
      |  Utf8 ([Ljava/lang/String;)V
      |  Utf8 java/lang/Object
      |  Utf8 <init>
      |  Utf8 ()V
      |  Class 23
      |  NameAndType 24 25
      |  Methodref 26.27
      |{
      |  Method;
      |    name: 24
      |    descriptor: 25
      |    flags: ACC_PUBLIC
      |    Code:
      |      name: 8, stack:1, locals:1
      |        aload 0
      |        invokespecial 28
      |        return
      |      Exceptions:
      |
      |  Method;
      |    name: 21
      |    descriptor: 22
      |    flags: ACC_STATIC, ACC_PUBLIC
      |    Code:
      |      name: 8, stack:2, locals:1
      |        getstatic 20
      |        iconst 5
      |        invokestatic 6
      |        invokevirtual 14
      |        return
      |      Exceptions:
      |
      |  Method;
      |    name: 2
      |    descriptor: 3
      |    flags: ACC_STATIC, ACC_PUBLIC
      |    Code:
      |      name: 8, stack:3, locals:1
      |        iload 0
      |        iconst 2
      |        if_icmpge 7
      |        iconst 1
      |        goto 16
      |        iload 0
      |        iconst 1
      |        isub
      |        invokestatic 6
      |        iload 0
      |        iconst 2
      |        isub
      |        invokestatic 6
      |        iadd
      |        ireturn
      |      StackMapTable: name: 7
      |        same frame, offset:9
      |        same locals, 1 stack item, offset:12
      |          int
      |      Exceptions:
      |}
    """.stripMargin

  private val emptyInlined =
    """class Empty extends java/lang/Object with: ()
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
      |}""".stripMargin

  private val emptyByteCode =
    """class 9 extends 5 with: ()
      |ConstantPool:
      |  Utf8 Empty
      |  Utf8 java/lang/Object
      |  Utf8 <init>
      |  Utf8 ()V
      |  Class 2
      |  NameAndType 3 4
      |  Methodref 5.6
      |  Utf8 Code
      |  Class 1
      |{
      |  Method;
      |    name: 3
      |    descriptor: 4
      |    flags: ACC_PUBLIC
      |    Code:
      |      name: 8, stack:1, locals:1
      |        aload 0
      |        invokespecial 7
      |        return
      |      Exceptions:
      |}""".stripMargin

  private val emptyByteCodeWithLineNumbers =
    """class 9 extends 5 with: ()
      |ConstantPool:
      |  #1 = Utf8 Empty
      |  #2 = Utf8 java/lang/Object
      |  #3 = Utf8 <init>
      |  #4 = Utf8 ()V
      |  #5 = Class 2
      |  #6 = NameAndType 3 4
      |  #7 = Methodref 5.6
      |  #8 = Utf8 Code
      |  #9 = Class 1
      |{
      |  Method;
      |    name: 3
      |    descriptor: 4
      |    flags: ACC_PUBLIC
      |    Code:
      |      name: 8, stack:1, locals:1
      |        aload 0
      |        invokespecial 7
      |        return
      |      Exceptions:
      |}""".stripMargin

  test("inlined bytecode parse & print") {
    val deltas = Seq(InlineConstantPool, PrettyPrint()) ++ ByteCodeLanguage.byteCodeDeltas
    new TestLanguageGrammarUtils(deltas).compareInputWithPrint(emptyInlined)
  }

  test("inline to numbered bytecode") {
    val deltas = Seq(InlineConstantPool, PrettyPrint(), ConstantPoolIndices) ++ ByteCodeLanguage.byteCodeDeltas
    val compiler = TestLanguageBuilder.buildWithParser(deltas)
    val result = new LanguageTest(compiler).compileAndPrettyPrint(emptyInlined)
    assertResult(emptyByteCodeWithLineNumbers)(result)
  }

  test("inline to bytecode") {
    val deltas = Seq(InlineConstantPool, PrettyPrint()) ++ ByteCodeLanguage.byteCodeDeltas
    val compiler = TestLanguageBuilder.buildWithParser(deltas)
    val result = new LanguageTest(compiler).compileAndPrettyPrint(emptyInlined)
    assertResult(emptyByteCode)(result)
  }
}
