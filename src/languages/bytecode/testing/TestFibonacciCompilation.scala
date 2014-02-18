package languages.bytecode.testing

import org.junit.{Assert, Test}
import languages.bytecode._
import transformation.{ComparisonOptions, MetaObject}
import languages.java.base.{JavaClassModel, JavaBaseModel, JavaBase}
import JavaBaseModel._
import JavaClassModel._
import util.TestConsole
import javaBytecode.{ByteCodeTypedUnTypedConversions, JavaByteCodeMachine}
import languages.java.base.JavaTypes._
import languages.java.base.JavaMethodModel._

class TestFibonacciCompilation {

  @Test
  def compareCompiledVersusNativeCode() {
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
      ByteCode.doReturn
    )
    val method = ByteCode.methodInfo(0,0,Seq(ByteCode.codeAttribute(0,0,0,0,instructions,Seq(),Seq())))
    val nativeClass = ByteCode.clazz(className, Seq(), Seq(method))
    Assert.assertTrue(MetaObject.deepEquality(compiledCode, nativeClass, new ComparisonOptions(false, false)))
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
      ByteCode.doReturn
    )
    val method = ByteCode.methodInfo(0,0,Seq(ByteCode.codeAttribute(0,0,0,0,instructions,Seq(),Seq())))
    val nativeClass = ByteCode.clazz(className, Seq(), Seq(method))
    Assert.assertTrue(MetaObject.deepEquality(compiledCode, nativeClass, new ComparisonOptions(false, false)))
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
    Assert.assertEquals("8",console.stdOut.toString())
  }

  def getMainMethod: MetaObject = {
    val parameters = Seq(parameter("args", arrayType(StringType)))
    val fibCall = call(variable("fibonacci"), Seq(LiteralC.literal(5)))
    val body = Seq(call(variable("Console.printf"), Seq(StringLiteralC.literal("%i"), fibCall)))
    method("main",VoidType, parameters, body, static = true, publicVisibility)
  }

  def getFibonacciMethod: MetaObject = {
    val parameters = Seq(parameter("i", IntegerType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"),LiteralC.literal(2))
    val body = Seq(TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2)))
    method("fibonacci", IntegerType, parameters, body, static = true)
  }
}
