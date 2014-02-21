package languages.java.testing

import org.junit.{Assert, Test}
import languages.bytecode._
import transformation.{ComparisonOptions, MetaObject}
import languages.java.base._
import JavaBaseModel._
import JavaClassModel._
import util.TestConsole
import javaBytecode.{ByteCodeTypedUnTypedConversions, JavaByteCodeMachine}
import languages.java.base.JavaTypes._
import languages.java.base.JavaMethodModel._
import languages.java._
import transformation.ComparisonOptions
import transformation.ComparisonOptions

class TestFibonacciCompilation {
  val className = "test"

  @Test
  def compareCompiledVersusNativeCodeInstructionsOnly() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getNativeUnoptimizedFibonacci()
    Assert.assertTrue(MetaObject.deepEquality(
      getClassMethodInstructions(compiledCode),
      getClassMethodInstructions(nativeClass),
      new ComparisonOptions(false, true, true)))
  }

  def getClassMethodInstructions(clazz: MetaObject) = {
    ByteCode.getMethodAnnotations(ByteCode.getMethods(clazz)(0))(0)
  }

  @Test
  def compareCompiledVersusNativeCode2() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getNativeUnoptimizedFibonacci()
    Assert.assertTrue(MetaObject.deepEquality(compiledCode, nativeClass,
      new ComparisonOptions(false, false, true)))
  }

  @Test
  def testDefaultConstructorGeneration() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getNativeUnoptimizedFibonacci()
    Assert.assertTrue(MetaObject.deepEquality(compiledCode, nativeClass, new ComparisonOptions(true, false, true)))
  }

  def getCompiledFibonacci: MetaObject = {
    val fibonacci = clazz(className, Seq(getFibonacciMethod))
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacci)
    compiledCode
  }

  def getNativeUnoptimizedFibonacci(): MetaObject = {
    val instructions = Seq(
      ByteCode.addressLoad(0),
      ByteCode.integerConstant(0),
      ByteCode.ifIntegerCompareGreater(9),
      ByteCode.integerConstant(1),
      ByteCode.goTo(22),
      ByteCode.integerConstant(0),
      ByteCode.ifIntegerCompareGreater(9),
      ByteCode.integerConstant(1),
      ByteCode.goTo(22),
      ByteCode.addressLoad(0),
      ByteCode.integerConstant(1),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.addressLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.addInteger,
      ByteCode.integerReturn
    )
    val methodName = "fibonacci"
    val constantPool = Seq(ByteCode.methodRef(4, 14),
      ByteCode.methodRef(3, 15),
      ByteCode.classRef(16),
      ByteCode.classRef(17),
      "<init>",
      ByteCode.methodDescriptor(JavaTypes.VoidType, Seq()),
      "Code",
      "LineNumberTable",
      methodName,
      ByteCode.methodDescriptor(JavaTypes.IntegerType, Seq(JavaTypes.IntegerType)),
      "StackMapTable",
      "SourceFile",
      "OnlyFibonacci.java",
      ByteCode.nameAndType(5, 6),
      ByteCode.nameAndType(9, 10),
      "languages/bytecode/testing/OnlyFibonacci",
      "java/lang/Object")
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0, 0, 0, 0, instructions, Seq(), Seq())))
    val nativeClass = ByteCode.clazz(className, constantPool, Seq(method))
    nativeClass
  }

  @Test
  def compareCompiledVersusOptimizedNativeCode() {
    val className = "test"
    val fibonacci = clazz(className, Seq(getFibonacciMethod))
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacci)
    val instructions = Seq(
      ByteCode.addressLoad(0),
      ByteCode.integerConstant(0),
      ByteCode.ifIntegerCompareGreater(9),
      ByteCode.integerConstant(1),
      ByteCode.goTo(22),
      ByteCode.addressLoad(0),
      ByteCode.integerConstant(1),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.addressLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.addInteger,
      ByteCode.integerReturn
    )
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0, 0, 0, 0, instructions, Seq(), Seq())))
    val nativeClass = ByteCode.clazz(className, Seq(), Seq(method))
    Assert.assertTrue(MetaObject.deepEquality(compiledCode, nativeClass, new ComparisonOptions(false, false, false)))
  }

  @Test
  def testCompiledCodeInterpretation() {
    val className = "test"
    val fibonacci = clazz(className, Seq(getFibonacciMethod))
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    val console = new TestConsole
    val machine = new JavaByteCodeMachine(console)
    val typedByteCode = ByteCodeTypedUnTypedConversions.toTyped(byteCode)
    machine.run(typedByteCode)
    Assert.assertEquals("8", console.stdOut.toString())
  }

  def getMainMethod: MetaObject = {
    val parameters = Seq(parameter("args", arrayType(StringType)))
    val fibCall = call(variable("fibonacci"), Seq(LiteralC.literal(5)))
    val body = Seq(call(variable("Console.printf"), Seq(StringLiteralC.literal("%i"), fibCall)))
    method("main", VoidType, parameters, body, static = true, PublicVisibility)
  }

  def getFibonacciMethod: MetaObject = {
    val parameters = Seq(parameter("i", IntegerType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"), LiteralC.literal(2))
    val body = Seq(JavaMethodModel._return(TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2))))
    method("fibonacci", IntegerType, parameters, body, static = true)
  }
}
