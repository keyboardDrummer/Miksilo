package deltas.javac

import core.deltas.path.PathRoot
import core.language.node.Node
import deltas.bytecode.types.IntTypeDelta
import deltas.expression.additive.{AdditionDelta, SubtractionDelta}
import deltas.expression.relational.LessThanDelta
import deltas.expression.{IntLiteralDelta, TernaryDelta, VariableDelta}
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.methods.{MethodDelta, MethodParameters, ReturnExpressionDelta}
import deltas.javac.methods.call.CallDelta
import deltas.statement.BlockDelta
import util.{LanguageTest, TestLanguageBuilder}

class FibonacciWithoutMain {
  val className = "OnlyFibonacci"
  val defaultPackage = Seq("transformations", "bytecode", "testing")
  val methodName = "fibonacci"

  def compileAndPrintFibonacciWithoutMain(): Unit = {
    val fibonacci = getJavaFibonacciWithoutMain
    val byteCode = TestLanguageBuilder.buildWithParser(JavaToByteCodeLanguage.javaCompilerDeltas).compileAst(fibonacci).program
    LanguageTest.printByteCode(byteCode.asInstanceOf[PathRoot].current)
  }

  def getJavaFibonacciWithoutMain: Node = {
    JavaClassDelta.neww(defaultPackage, className, Seq(getFibonacciMethodJava))
  }

  def getFibonacciMethodJava: Node = {
    val parameters = Seq(MethodParameters.neww("i", IntTypeDelta.intType))
    val recursiveCall1 = CallDelta.neww(VariableDelta.neww("fibonacci"), Seq(SubtractionDelta.neww(VariableDelta.neww("i"), IntLiteralDelta.neww(1))))
    val recursiveCall2 = CallDelta.neww(VariableDelta.neww("fibonacci"), Seq(SubtractionDelta.neww(VariableDelta.neww("i"), IntLiteralDelta.neww(2))))
    val condition = LessThanDelta.neww(VariableDelta.neww("i"), IntLiteralDelta.neww(2))
    val returnValue = TernaryDelta.ternary(condition, IntLiteralDelta.neww(1), AdditionDelta.neww(recursiveCall1, recursiveCall2))
    val body =  BlockDelta.neww(Seq(ReturnExpressionDelta.neww(returnValue)))
    MethodDelta.neww("fibonacci", IntTypeDelta.intType, parameters, body, static = true)
  }
}
