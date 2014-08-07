package transformations.javac

import core.transformation.MetaObject
import org.junit.{Assert, Test}
import transformations.bytecode._
import transformations.javac.base.model.JavaClassModel._
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model._
import transformations.javac.expressions.LiteralC
import transformations.javac.methods.{CallC, SelectorC, VariableC}
import transformations.javac.types.{ArrayTypeC, ObjectTypeC, VoidTypeC}

import scala.reflect.io.Path

class TestFibonacciWithMain {
  val defaultPackage = Seq()
  val className = "Fibonacci"
  val other = new FibonacciWithoutMain()

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

}
