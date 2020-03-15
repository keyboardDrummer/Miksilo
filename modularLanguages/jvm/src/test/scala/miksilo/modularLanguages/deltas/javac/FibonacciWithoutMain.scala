package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta
import miksilo.modularLanguages.deltas.expression.additive.{AdditionDelta, SubtractionDelta}
import miksilo.modularLanguages.deltas.expression.relational.LessThanDelta
import miksilo.modularLanguages.deltas.expression.{IntLiteralDelta, TernaryDelta, VariableDelta}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.methods.{MethodParameters, ReturnExpressionDelta}
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta
import miksilo.modularLanguages.deltas.statement.BlockDelta
import miksilo.modularLanguages.util.TestLanguageBuilder
import miksilo.modularLanguagesutil.LanguageTest

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
