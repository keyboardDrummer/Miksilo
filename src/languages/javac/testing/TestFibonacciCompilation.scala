package languages.javac.testing

import org.junit.{Assert, Test}
import languages.bytecode._
import transformation.MetaObject
import languages.javac.base._
import JavaBaseModel._
import JavaClassModel._
import util.TestConsole
import javaBytecode.{ByteCodeTypedUnTypedConversions, JavaByteCodeMachine}
import languages.javac.base.JavaTypes._
import languages.javac.base.JavaMethodModel._
import languages.javac._
import transformation.ComparisonOptions
import scala.reflect.io.File

class TestFibonacciCompilation {
  val className = "OnlyFibonacci"

  @Test
  def compareCompiledVersusNativeCodeFibonacciInstructionsOnly() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getExpectedUnoptimizedFibonacciWithoutMainByteCode
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
    val nativeClass: MetaObject = getExpectedUnoptimizedFibonacciWithoutMainByteCode
    TestUtils.compareConstantPools(nativeClass, compiledCode)
    TestUtils.testMethodEquivalence(nativeClass, compiledCode)
  }

  def getCompiledFibonacci: MetaObject = {
    val fibonacci = getJavaFibonacciWithoutMain
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacci)
    compiledCode
  }


  def getJavaFibonacciWithoutMain: MetaObject = {
    clazz(defaultPackage, className, Seq(getFibonacciMethodByteCode))
  }

  def getJavaFibonacciWithMain: MetaObject = {
    clazz(defaultPackage, className, Seq(getMainMethod, getFibonacciMethodByteCode))
  }

  def getConstructorByteCode : MetaObject = {
    val instructions = Seq(ByteCode.integerLoad(0), ByteCode.invokeSpecial(1), ByteCode.voidReturn)
    val codeAttribute = Seq(ByteCode.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    ByteCode.methodInfo(3,4, codeAttribute, Set(ByteCode.PublicAccess))
  }

  def getExpectedUnoptimizedFibonacciWithoutMainByteCode: MetaObject = {
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
    val nativeClass = ByteCode.clazz(3, 4, constantPool, Seq(getConstructorByteCode, method))
    nativeClass
  }

  val defaultPackage = Seq("languages","bytecode","testing")

  def getMainByteCode = {
    val instructions = Seq(ByteCode.getStatic(2),
      ByteCode.integerConstant(5),
      ByteCode.invokeStatic(3),
      ByteCode.invokeVirtual(4),
      ByteCode.voidReturn)
    ByteCode.methodInfo(11,12,Seq(ByteCode.codeAttribute(9,2,1,instructions,Seq(),Seq())),
      Set(ByteCode.PublicAccess,ByteCode.StaticAccess))
  }

  @Test
  def compileAndValidateFibonacciWithMain() {
    val fibonacci = getJavaFibonacciWithMain
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    val compiledMainByteCode = ByteCode.getMethods(byteCode)(1)
    val expectedMainByteCode = getMainByteCode
    Assert.assertTrue(MetaObject.deepEquality(compiledMainByteCode,expectedMainByteCode,
      new ComparisonOptions(false,false,true)))
  }

  @Test
  def compileAndPrintFibonacciWithMain() {
    val fibonacci = getJavaFibonacciWithMain
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.compile(fibonacci)
    PrintByteCode.print(compiledCode)
  }

  @Test
  def testCompiledCodeInterpretation() {
    val fibonacci = getJavaFibonacciWithoutMain
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    val console = new TestConsole
    val machine = new JavaByteCodeMachine(console)
    val typedByteCode = ByteCodeTypedUnTypedConversions.toTyped(byteCode)
    machine.run(typedByteCode)
    Assert.assertEquals("8", console.stdOut.toString())
  }

  @Test
  def compileAndPrintFibonacciWithoutMain() {
    val fibonacci = getJavaFibonacciWithoutMain
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    PrintByteCode.print(byteCode)
  }

  @Test
  def runCompiledFibonacci() {
    val fibonacci = getJavaFibonacciWithoutMain
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    val bytes = PrintByteCode.print(byteCode)
    val fileName = "test.class"
    val file = File(fileName)
    file.bufferedWriter().append(bytes)

    val processBuilder = new ProcessBuilder("java",fileName)
    val redirect = processBuilder.redirectOutput()
    processBuilder.start()
  }

  def getMainMethod: MetaObject = {
    val parameters = Seq(parameter("args", arrayType(objectType(new QualifiedClassName(Seq("java","lang","String"))))))
    val fibCall = call(variable("fibonacci"), Seq(LiteralC.literal(5)))
    val body = Seq(call(selector(selector(selector(selector(variable("java"),"lang"),"System"),"out"),"print"), Seq(fibCall)))
    method("main", VoidType, parameters, body, static = true, PublicVisibility)
  }

  def getFibonacciMethodByteCode: MetaObject = {
    val parameters = Seq(parameter("i", IntegerType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"), LiteralC.literal(2))
    val returnValue = TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2))
    val body = Seq(JavaMethodModel._return(Some(returnValue)))
    method("fibonacci", IntegerType, parameters, body, static = true)
  }
}
