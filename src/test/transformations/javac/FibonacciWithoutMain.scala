package transformations.javac

import core.transformation.MetaObject
import org.junit.Test
import transformations.javac.base.model.JavaClassModel._
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.expressions._
import transformations.javac.methods.CallC._
import transformations.javac.methods.ReturnExpressionC
import transformations.javac.methods.VariableC._
import transformations.javac.types.IntTypeC

class FibonacciWithoutMain {
  val className = "OnlyFibonacci"
  val defaultPackage = Seq("transformations", "bytecode", "testing")
  val methodName = "fibonacci"

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
    val parameters = Seq(parameter("i", IntTypeC.intType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), NumberLiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), NumberLiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"), NumberLiteralC.literal(2))
    val returnValue = TernaryC.ternary(condition, NumberLiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2))
    val body = Seq(ReturnExpressionC._return(returnValue))
    method("fibonacci", IntTypeC.intType, parameters, body, static = true)
  }
}
