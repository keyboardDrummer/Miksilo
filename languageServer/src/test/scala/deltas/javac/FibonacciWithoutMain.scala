package deltas.javac

import core.language.node.Node
import deltas.bytecode.types.IntTypeDelta
import deltas.expression.IntLiteralDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions._
import deltas.javac.expressions.additive.{AdditionDelta, SubtractionDelta}
import deltas.javac.expressions.relational.LessThanDelta
import deltas.javac.methods.MethodDelta._
import deltas.javac.methods.ReturnExpressionDelta
import deltas.expressions.VariableDelta._
import deltas.javac.methods.call.CallDelta._
import util.TestLanguageBuilder
import util.TestUtils

class FibonacciWithoutMain {
  val className = "OnlyFibonacci"
  val defaultPackage = Seq("transformations", "bytecode", "testing")
  val methodName = "fibonacci"

  def compileAndPrintFibonacciWithoutMain() {
    val fibonacci = getJavaFibonacciWithoutMain
    val byteCode = TestLanguageBuilder.build(JavaLanguage.javaCompilerDeltas).transform(fibonacci).program
    TestUtils.printByteCode(byteCode)
  }

  def getJavaFibonacciWithoutMain: Node = {
    neww(defaultPackage, className, Seq(getFibonacciMethodJava))
  }

  def getFibonacciMethodJava: Node = {
    val parameters = Seq(parameter("i", IntTypeDelta.intType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionDelta.subtraction(variable("i"), IntLiteralDelta.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionDelta.subtraction(variable("i"), IntLiteralDelta.literal(2))))
    val condition = LessThanDelta.neww(variable("i"), IntLiteralDelta.literal(2))
    val returnValue = TernaryDelta.ternary(condition, IntLiteralDelta.literal(1), AdditionDelta.addition(recursiveCall1, recursiveCall2))
    val body = Seq(ReturnExpressionDelta._return(returnValue))
    method("fibonacci", IntTypeDelta.intType, parameters, body, static = true)
  }
}
