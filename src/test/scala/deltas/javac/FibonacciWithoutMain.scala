package deltas.javac

import core.deltas.node.Node
import deltas.bytecode.types.IntTypeC
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.expressions._
import deltas.javac.expressions.additive.{AdditionDelta, SubtractionC}
import deltas.javac.expressions.literals.IntLiteralDelta
import deltas.javac.expressions.relational.LessThanC
import deltas.javac.methods.MethodDelta._
import deltas.javac.methods.ReturnExpressionDelta
import deltas.javac.methods.VariableDelta._
import deltas.javac.methods.call.CallC._
import util.CompilerBuilder
import util.TestUtils

class FibonacciWithoutMain {
  val className = "OnlyFibonacci"
  val defaultPackage = Seq("transformations", "bytecode", "testing")
  val methodName = "fibonacci"


  def compileAndPrintFibonacciWithoutMain() {
    val fibonacci = getJavaFibonacciWithoutMain
    val byteCode = CompilerBuilder.build(JavaCompilerDeltas.javaCompilerDeltas).transform(fibonacci).program
    TestUtils.printByteCode(byteCode)
  }

  def getJavaFibonacciWithoutMain: Node = {
    clazz(defaultPackage, className, Seq(getFibonacciMethodJava))
  }

  def getFibonacciMethodJava: Node = {
    val parameters = Seq(parameter("i", IntTypeC.intType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), IntLiteralDelta.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), IntLiteralDelta.literal(2))))
    val condition = LessThanC.lessThan(variable("i"), IntLiteralDelta.literal(2))
    val returnValue = TernaryDelta.ternary(condition, IntLiteralDelta.literal(1), AdditionDelta.addition(recursiveCall1, recursiveCall2))
    val body = Seq(ReturnExpressionDelta._return(returnValue))
    method("fibonacci", IntTypeC.intType, parameters, body, static = true)
  }
}
