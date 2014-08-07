package transformations.javac

import core.transformation.MetaObject
import org.junit.{Assert, Test}
import transformations.bytecode._
import transformations.bytecode.instructions._
import transformations.bytecode.instructions.integerCompare.{IfIntegerCompareGreaterOrEqualC, IfZeroC}
import transformations.javac.base.model.JavaClassModel._
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model._
import transformations.javac.expressions.LiteralC
import transformations.javac.methods.{CallC, SelectorC, VariableC}
import transformations.javac.types.{ArrayTypeC, IntTypeC, ObjectTypeC, VoidTypeC}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Path

class FibonacciWithMain {
  val defaultPackage = Seq()
  val className = "Fibonacci"
  val other = new FibonacciWthoutMain()

  val expectedOutput: Int = 8
  val methodName = "fibonacci"

  @Test
  def testFullPipeline() {
    val inputDirectory = Path("fibonacciWithMain")
    val output: String = TestUtils.compileAndRun(className, inputDirectory)
    Assert.assertEquals(expectedOutput, Integer.parseInt(output))
  }

  @Test
  def runCompiledFibonacci() {
    val fibonacci = getJavaFibonacciWithMain
    val byteCode = JavaCompiler.getTransformer.transform(fibonacci)

    val expectedResult = expectedOutput
    TestUtils.runByteCode(className, byteCode, expectedResult)
  }

  @Test
  def testStackSizeLocalsArgs() {
    val fibonacci = getJavaFibonacciWithMain
    val byteCode = JavaCompiler.getTransformer.transform(fibonacci)
    val methods = ByteCodeSkeleton.getMethods(byteCode)
    val constructor = methods(0)
    val main = methods(1)
    val fibonacciMethod = methods(2)
    Assert.assertEquals(1, getMethodMaxStack(constructor))
    Assert.assertEquals(2, getMethodMaxStack(main))
    Assert.assertEquals(3, getMethodMaxStack(fibonacciMethod))
    Assert.assertEquals(1, getMethodLocals(constructor))
    Assert.assertEquals(1, getMethodLocals(main))
    Assert.assertEquals(1, getMethodLocals(fibonacciMethod))

  }

  def getMethodLocals(method: MetaObject) = ByteCodeSkeleton.getCodeMaxLocals(ByteCodeSkeleton.getMethodAttributes(method)(0))

  def getMethodMaxStack(method: MetaObject) = ByteCodeSkeleton.getCodeMaxStack(ByteCodeSkeleton.getMethodAttributes(method)(0))

  @Test
  def compileAndPrintFibonacciWithMain() {
    val fibonacci = getJavaFibonacciWithMain
    val compiledCode = JavaCompiler.getTransformer.transform(fibonacci)
    TestUtils.printByteCode(compiledCode)
  }

  def getJavaFibonacciWithMain: MetaObject = {
    clazz(defaultPackage, className, Seq(getMainMethodJava, other.getFibonacciMethodJava))
  }

  def getMainMethodJava: MetaObject = {
    val parameters = Seq(parameter("args", ArrayTypeC.arrayType(ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String"))))))
    val fibCall = CallC.call(VariableC.variable("fibonacci"), Seq(LiteralC.literal(5)))
    val body = Seq(CallC.call(SelectorC.selector(SelectorC.selector(SelectorC.selector(SelectorC.selector(
      VariableC.variable("java"), "lang"), "System"), "out"), "print"), Seq(fibCall)))
    method("main", VoidTypeC.voidType, parameters, body, static = true, PublicVisibility)
  }

  @Test
  def compileAndValidateFibonacciWithMain() {
    val fibonacciJava = getJavaFibonacciWithMain
    val compiledCode = JavaCompiler.getTransformer.transform(fibonacciJava)
    val expectedCode = getExpectedUnoptimizedFibonacciWithMainByteCode
    TestUtils.testInstructionEquivalence(expectedCode, compiledCode)
    TestUtils.compareConstantPools(expectedCode, compiledCode)
  }

  def getExpectedUnoptimizedFibonacciWithMainByteCode: MetaObject = {
    val constantPool: ArrayBuffer[Any] = getConstantPool
    val method: MetaObject = getFibonacciMethodByteCode
    val nativeClass = ByteCodeSkeleton.clazz(3, 4, constantPool, Seq(other.getConstructorByteCode, getMainByteCode, method))
    nativeClass
  }

  def getMainByteCode = {
    val instructions = Seq(GetStaticC.getStatic(2),
      IntegerConstantC.integerConstant(5),
      InvokeStaticC.invokeStatic(3),
      InvokeVirtualC.invokeVirtual(4),
      VoidReturnC.voidReturn)
    ByteCodeSkeleton.methodInfo(11, 12, Seq(ByteCodeSkeleton.codeAttribute(9, 2, 1, instructions, Seq(), Seq())),
      Set(ByteCodeSkeleton.PublicAccess, ByteCodeSkeleton.StaticAccess))
  }

  def getFibonacciMethodByteCode: MetaObject = {
    val instructions = Seq(
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(2),
      IfIntegerCompareGreaterOrEqualC.ifIntegerCompareGreater(3),
      IntegerConstantC.integerConstant(1),
      GotoC.goTo(16),
      IntegerConstantC.integerConstant(0),
      IfZeroC.ifZero(7),
      IntegerConstantC.integerConstant(1),
      GotoC.goTo(16),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(1),
      SubtractIntegerC.subtractInteger,
      InvokeStaticC.invokeStatic(3),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(2),
      SubtractIntegerC.subtractInteger,
      InvokeStaticC.invokeStatic(3),
      AddIntegersC.addInteger,
      IntegerReturnC.integerReturn
    )
    val method = ByteCodeSkeleton.methodInfo(13, 14, Seq(ByteCodeSkeleton.codeAttribute(9, 3, 1, instructions, Seq(), Seq())),
      Set(ByteCodeSkeleton.PublicAccess, ByteCodeSkeleton.StaticAccess))
    method
  }

  def getConstantPool: ArrayBuffer[Any] = {
    val constantPool = ArrayBuffer[Any](ByteCodeSkeleton.methodRef(6, 18),
      ByteCodeSkeleton.fieldRef(19, 20),
      ByteCodeSkeleton.methodRef(5, 21),
      ByteCodeSkeleton.methodRef(22, 23),
      ByteCodeSkeleton.classRef(24),
      ByteCodeSkeleton.classRef(25),
      ConstructorC.constructorName,
      ByteCodeSkeleton.methodDescriptor(VoidTypeC.voidType, Seq()),
      ByteCodeSkeleton.CodeAttributeId,
      "main",
      ByteCodeSkeleton.methodDescriptor(VoidTypeC.voidType, Seq(
        ArrayTypeC.arrayType(ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))))),
      methodName,
      ByteCodeSkeleton.methodDescriptor(IntTypeC.intType, Seq(IntTypeC.intType)),
      ByteCodeSkeleton.StackMapTableId,
      ByteCodeSkeleton.nameAndType(7, expectedOutput),
      ByteCodeSkeleton.classRef(26),
      ByteCodeSkeleton.nameAndType(27, 28),
      ByteCodeSkeleton.nameAndType(13, 14),
      ByteCodeSkeleton.classRef(29),
      ByteCodeSkeleton.nameAndType(30, 31),
      new QualifiedClassName(defaultPackage ++ Seq(className)),
      new QualifiedClassName(Seq("java", "lang", "Object")),
      new QualifiedClassName(Seq("java", "lang", "System")),
      "out",
      ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "io", "PrintStream"))),
      new QualifiedClassName(Seq("java", "io", "PrintStream")),
      "print",
      ByteCodeSkeleton.methodDescriptor(VoidTypeC.voidType, Seq(IntTypeC.intType))
    )
    constantPool
  }
}
