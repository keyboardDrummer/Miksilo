package deltas.javac

import core.language.node.Node
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, VoidTypeDelta}
import deltas.javac.classes.skeleton.{JavaClassDelta, QualifiedClassName}
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta, MethodParameters}
import deltas.statement.BlockDelta
import org.scalatest.FunSuite
import util.{SourceUtils, TestLanguageBuilder}

class EmptyMain extends FunSuite {
  val className = "EmptyMain"
  val defaultPackage = Seq()
  val other = new FibonacciWithoutMain()

  test("runCompiledCode") {
    val byteCode: Node = getByteCode
    SourceUtils.runByteCode(className, byteCode)
  }

  def getByteCode: Node = {
    val java = getJava
    val byteCode = TestLanguageBuilder.build(JavaToByteCodeLanguage.javaCompilerDeltas).compileAst(java).program
    byteCode
  }

  def getJava: Node = {
    JavaClassDelta.neww(defaultPackage, className, Seq(getMainMethodJava))
  }

  def getMainMethodJava: Node = {
    val parameters = Seq(MethodParameters.neww("args", ArrayTypeDelta.neww(QualifiedObjectTypeDelta.neww(QualifiedClassName(Seq("java", "lang", "String"))))))
    MethodDelta.neww("main", VoidTypeDelta.voidType, parameters,  BlockDelta.neww(), static = true, AccessibilityFieldsDelta.PublicVisibility)
  }
}
