package transformations.javac

import core.transformation.{ComparisonOptions, MetaObject}
import org.junit.{Assert, Test}
import transformations.bytecode._
import transformations.bytecode.instructions._
import transformations.javac.base.model.JavaClassModel._
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model._
import transformations.javac.expressions._
import transformations.javac.methods.CallC._
import transformations.javac.methods.ReturnC
import transformations.javac.methods.VariableC._

import scala.collection.mutable.ArrayBuffer

class FibonacciWthoutMain {
  val className = "OnlyFibonacci"
  val defaultPackage = Seq("transformations", "bytecode", "testing")
  val methodName = "fibonacci"

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
    ByteCodeSkeleton.getCodeInstructions(ByteCodeSkeleton.getMethodAttributes(ByteCodeSkeleton.getMethods(clazz)(1))(0))
  }

  def getCompiledFibonacci: MetaObject = {
    val fibonacci = getJavaFibonacciWithoutMain
    val compiledCode = JavaCompiler.getTransformer.transform(fibonacci)
    compiledCode
  }

  def getExpectedUnoptimizedFibonacciWithoutMainByteCode: MetaObject = {
    val constantPool: ArrayBuffer[Any] = getConstantPool
    val method: MetaObject = getFibonacciMethodByteCode
    val nativeClass = ByteCodeSkeleton.clazz(3, 4, constantPool, Seq(getConstructorByteCode, method))
    nativeClass
  }

  def getConstructorByteCode: MetaObject = {
    val instructions = Seq(LoadAddressC.addressLoad(0), InvokeSpecialC.invokeSpecial(1), VoidReturnC.voidReturn)
    val codeAttribute = Seq(ByteCodeSkeleton.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    ByteCodeSkeleton.methodInfo(3, 4, codeAttribute, Set(ByteCodeSkeleton.PublicAccess))
  }

  def getFibonacciMethodByteCode: MetaObject = {
    val instructions = Seq(
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(2),
      IfIntegerCompareGreaterC.ifIntegerCompareGreater(9),
      IntegerConstantC.integerConstant(1),
      GotoC.goTo(22),
      IntegerConstantC.integerConstant(0),
      IfZeroC.ifZero(9),
      IntegerConstantC.integerConstant(1),
      GotoC.goTo(22),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(1),
      SubtractIntegerC.subtractInteger,
      InvokeStaticC.invokeStatic(2),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(2),
      SubtractIntegerC.subtractInteger,
      InvokeStaticC.invokeStatic(2),
      AddIntegersC.addInteger,
      IntegerReturnC.integerReturn
    )
    val stackMapTable = ByteCodeSkeleton.stackMapTable(14, Seq(ByteCodeSkeleton.sameFrame(9),
      ByteCodeSkeleton.sameFrameLocals1StackItem(12, JavaTypes.intType)))
    val codeAttribute = ByteCodeSkeleton.codeAttribute(0, 0, 0, instructions, Seq(), Seq(stackMapTable))
    ByteCodeSkeleton.methodInfo(0, 0, Seq(codeAttribute), Set(ByteCodeSkeleton.PrivateAccess, ByteCodeSkeleton.StaticAccess))
  }

  def getConstantPool: ArrayBuffer[Any] = {
    val constantPool = ArrayBuffer[Any](ByteCodeSkeleton.methodRef(4, 11),
      ByteCodeSkeleton.methodRef(3, 12),
      ByteCodeSkeleton.classRef(13),
      ByteCodeSkeleton.classRef(14),
      ConstructorC.constructorName,
      ByteCodeSkeleton.methodDescriptor(JavaTypes.voidType, Seq()),
      ByteCodeSkeleton.CodeAttributeId,
      methodName,
      ByteCodeSkeleton.methodDescriptor(JavaTypes.intType, Seq(JavaTypes.intType)),
      ByteCodeSkeleton.nameAndType(5, 6),
      ByteCodeSkeleton.nameAndType(8, 9),
      new QualifiedClassName(Seq("transformations", "bytecode", "testing", "OnlyFibonacci")),
      new QualifiedClassName(Seq("java", "lang", "Object")),
      ByteCodeSkeleton.StackMapTableId)
    constantPool
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val compiledCode: MetaObject = getCompiledFibonacci
    val nativeClass: MetaObject = getExpectedUnoptimizedFibonacciWithoutMainByteCode
    TestUtils.testInstructionEquivalence(nativeClass, compiledCode)
    TestUtils.compareConstantPools(nativeClass, compiledCode)
  }

  @Test
  def compileAndPrintFibonacciWithoutMain() {
    val fibonacci = getJavaFibonacciWithoutMain
    val byteCode = JavaCompiler.getTransformer.transform(fibonacci)
    TestUtils.printByteCode(byteCode)
  }

  def getJavaFibonacciWithoutMain: MetaObject = {
    clazz(defaultPackage, className, Seq(getFibonacciMethodJava))
  }

  def getFibonacciMethodJava: MetaObject = {
    val parameters = Seq(parameter("i", JavaTypes.intType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"), LiteralC.literal(2))
    val returnValue = TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2))
    val body = Seq(ReturnC._return(Some(returnValue)))
    method("fibonacci", JavaTypes.intType, parameters, body, static = true)
  }
}
