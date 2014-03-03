package languages.javac.testing

import org.junit.{Assert, Test}
import languages.bytecode._
import transformation.{ComparisonOptions, MetaObject}
import languages.javac.base._
import JavaBaseModel._
import JavaClassModel._
import util.TestConsole
import javaBytecode.{ByteCodeTypedUnTypedConversions, JavaByteCodeMachine}
import languages.javac.base.JavaTypes._
import languages.javac.base.JavaMethodModel._
import languages.javac._
import transformation.ComparisonOptions
import transformation.ComparisonOptions

class TestFibonacciCompilation {
  val className = "OnlyFibonacci"

  @Test
  def compareCompiledVersusNativeCodeFibonacciInstructionsOnly() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getNativeUnoptimizedFibonacci()
    Assert.assertTrue(MetaObject.deepEquality(
      getClassMethodInstructions(compiledCode),
      getClassMethodInstructions(nativeClass),
      new ComparisonOptions(false, true, true)))
  }

  def getClassMethodInstructions(clazz: MetaObject) = {
    ByteCode.getMethodAttributes(ByteCode.getMethods(clazz)(1))(0)
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getNativeUnoptimizedFibonacci()
    TestUtils.compareConstantPools(nativeClass, compiledCode)
    TestUtils.testMethodEquivalence(nativeClass, compiledCode)
  }

  def getCompiledFibonacci: MetaObject = {
    val fibonacci = getJavaFibonacci
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacci)
    compiledCode
  }


  def getJavaFibonacci: MetaObject = {
    clazz(defaultPackage, className, Seq(getFibonacciMethod))
  }

  def getConstructorByteCode() : MetaObject = {
    val instructions = Seq(ByteCode.integerLoad(0), ByteCode.invokeSpecial(1), ByteCode.voidReturn)
    val codeAttribute = Seq(ByteCode.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    ByteCode.methodInfo(3,4, codeAttribute)
  }

  def getNativeUnoptimizedFibonacci(): MetaObject = {
    val instructions = Seq(
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(0),
      ByteCode.ifIntegerCompareGreater(9),
      ByteCode.integerConstant(1),
      ByteCode.goTo(22),
      ByteCode.integerConstant(0),
      ByteCode.ifIntegerCompareGreater(9),
      ByteCode.integerConstant(1),
      ByteCode.goTo(22),
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(1),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.addInteger,
      ByteCode.integerReturn
    )
    val methodName = "fibonacci"
    val constantPool = Seq(ByteCode.methodRef(4, 11),
      ByteCode.methodRef(3, 12),
      ByteCode.classRef(13),
      ByteCode.classRef(14),
      ConstructorC.constructorName,
      ByteCode.methodDescriptor(JavaTypes.VoidType, Seq()),
      ByteCode.CodeAttributeId,
      methodName,
      ByteCode.methodDescriptor(JavaTypes.IntegerType, Seq(JavaTypes.IntegerType)),
      ByteCode.nameAndType(5, 6),
      ByteCode.nameAndType(8, 9),
      new QualifiedClassName(Seq("languages","bytecode","testing","OnlyFibonacci")),
      new QualifiedClassName(Seq("java","lang","Object")))
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    val nativeClass = ByteCode.clazz(3, 4, constantPool, Seq(getConstructorByteCode(), method))
    nativeClass
  }

  val defaultPackage = Seq("languages","bytecode","testing")
  @Test
  def compareCompiledVersusOptimizedNativeCode() {
    val fibonacci = clazz(defaultPackage, className, Seq(getConstructorByteCode(), getFibonacciMethod))
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacci)
    val instructions = Seq(
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(0),
      ByteCode.ifIntegerCompareGreater(9),
      ByteCode.integerConstant(1),
      ByteCode.goTo(22),
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(1),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.subtractInteger,
      ByteCode.invokeStatic(2),
      ByteCode.addInteger,
      ByteCode.integerReturn
    )
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    val nativeClass = ByteCode.clazz(0, 0, Seq(), Seq(getConstructorByteCode(), method))
    Assert.assertTrue(MetaObject.deepEquality(compiledCode, nativeClass, new ComparisonOptions(false, false, false)))
  }

  @Test
  def compileAndPrintFibonacci() {
    val fibonacci = getJavaFibonacci
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    PrintByteCode.print(byteCode)
  }

  @Test
  def testCompiledCodeInterpretation() {
    val fibonacci = getJavaFibonacci
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
    val returnValue = TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2))
    val body = Seq(JavaMethodModel._return(Some(returnValue)))
    method("fibonacci", IntegerType, parameters, body, static = true)
  }
}
