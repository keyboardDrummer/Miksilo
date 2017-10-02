package transformations.javac

import core.particles.node.Node
import transformations.bytecode.types.IntTypeC
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AdditionC, SubtractionC}
import transformations.javac.expressions.literals.IntLiteralC
import transformations.javac.expressions.relational.LessThanC
import transformations.javac.methods.MethodC._
import transformations.javac.methods.ReturnExpressionC
import transformations.javac.methods.VariableC._
import transformations.javac.methods.call.CallC._
import util.CompilerBuilder
import util.TestUtils

class FibonacciWithoutMain {
  val className = "OnlyFibonacci"
  val defaultPackage = Seq("transformations", "bytecode", "testing")
  val methodName = "fibonacci"


  def compileAndPrintFibonacciWithoutMain() {
    val fibonacci = getJavaFibonacciWithoutMain
    val byteCode = CompilerBuilder.build(JavaCompiler.javaCompilerTransformations).transform(fibonacci).program
    TestUtils.printByteCode(byteCode)
  }

  def getJavaFibonacciWithoutMain: Node = {
    clazz(defaultPackage, className, Seq(getFibonacciMethodJava))
  }

  def getFibonacciMethodJava: Node = {
    val parameters = Seq(parameter("i", IntTypeC.intType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), IntLiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), IntLiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"), IntLiteralC.literal(2))
    val returnValue = TernaryC.ternary(condition, IntLiteralC.literal(1), AdditionC.addition(recursiveCall1, recursiveCall2))
    val body = Seq(ReturnExpressionC._return(returnValue))
    method("fibonacci", IntTypeC.intType, parameters, body, static = true)
  }
}
