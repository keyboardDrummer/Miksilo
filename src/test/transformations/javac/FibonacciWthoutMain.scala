package transformations.javac

import core.transformation.{ComparisonOptions, MetaObject}
import org.junit.{Assert, Test}
import transformations.bytecode._
import transformations.javac.base.model.JavaBaseModel._
import transformations.javac.base.model.JavaClassModel._
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model.JavaTypes._
import transformations.javac.base.model._
import transformations.javac.expressions._

import scala.collection.mutable.ArrayBuffer

class FibonacciWthoutMain {
  val className = "OnlyFibonacci"
  val defaultPackage = Seq("transformations", "bytecode", "testing")

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
    ByteCode.getCodeInstructions(ByteCode.getMethodAttributes(ByteCode.getMethods(clazz)(1))(0))
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getExpectedUnoptimizedFibonacciWithoutMainByteCode
    TestUtils.testInstructionEquivalence(nativeClass, compiledCode)
    TestUtils.compareConstantPools(nativeClass, compiledCode)
  }

  def getCompiledFibonacci: MetaObject = {
    val fibonacci = getJavaFibonacciWithoutMain
    val compiler = JavaCompiler.getCompiler
    val compiledCode = compiler.transform(fibonacci)
    compiledCode
  }

  def getJavaFibonacciWithoutMain: MetaObject = {
    clazz(defaultPackage, className, Seq(getFibonacciMethodJava))
  }

  def getConstructorByteCode: MetaObject = {
    val instructions = Seq(ByteCode.addressLoad(0), ByteCode.invokeSpecial(1), ByteCode.voidReturn)
    val codeAttribute = Seq(ByteCode.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    ByteCode.methodInfo(3, 4, codeAttribute, Set(ByteCode.PublicAccess))
  }

  val methodName = "fibonacci"

  def getExpectedUnoptimizedFibonacciWithoutMainByteCode: MetaObject = {
    val constantPool: ArrayBuffer[Any] = getConstantPool
    val method: MetaObject = getFibonacciMethodByteCode
    val nativeClass = ByteCode.clazz(3, 4, constantPool, Seq(getConstructorByteCode, method))
    nativeClass
  }


  def getFibonacciMethodByteCode: MetaObject = {
    val instructions = Seq(
      ByteCode.integerLoad(0),
      ByteCode.integerConstant(2),
      ByteCode.ifIntegerCompareGreater(9),
      ByteCode.integerConstant(1),
      ByteCode.goTo(22),
      ByteCode.integerConstant(0),
      ByteCode.ifZero(9),
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
    val stackMapTable = ByteCode.stackMapTable(14, Seq(ByteCode.sameFrame(9),
      ByteCode.sameFrameLocals1StackItem(12, Seq(JavaTypes.IntType))))
    val codeAttribute = ByteCode.codeAttribute(0, 0, 0, instructions, Seq(), Seq(stackMapTable))
    ByteCode.methodInfo(0, 0, Seq(codeAttribute), Set(ByteCode.PrivateAccess, ByteCode.StaticAccess))
  }

  def getConstantPool: ArrayBuffer[Any] = {
    val constantPool = ArrayBuffer[Any](ByteCode.methodRef(4, 11),
      ByteCode.methodRef(3, 12),
      ByteCode.classRef(13),
      ByteCode.classRef(14),
      ConstructorC.constructorName,
      ByteCode.methodDescriptor(JavaTypes.VoidType, Seq()),
      ByteCode.CodeAttributeId,
      methodName,
      ByteCode.methodDescriptor(JavaTypes.IntType, Seq(JavaTypes.IntType)),
      ByteCode.nameAndType(5, 6),
      ByteCode.nameAndType(8, 9),
      new QualifiedClassName(Seq("transformations", "bytecode", "testing", "OnlyFibonacci")),
      new QualifiedClassName(Seq("java", "lang", "Object")),
      ByteCode.StackMapTableId)
    constantPool
  }

  @Test
  def compileAndPrintFibonacciWithoutMain() {
    val fibonacci = getJavaFibonacciWithoutMain
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.transform(fibonacci)
    PrintByteCode.print(byteCode)
  }

  def getFibonacciMethodJava: MetaObject = {
    val parameters = Seq(parameter("i", IntType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"), LiteralC.literal(2))
    val returnValue = TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2))
    val body = Seq(JavaMethodModel._return(Some(returnValue)))
    method("fibonacci", IntType, parameters, body, static = true)
  }
}
