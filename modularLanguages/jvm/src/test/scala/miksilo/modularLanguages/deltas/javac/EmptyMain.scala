package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, VoidTypeDelta}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{JavaClassDelta, QualifiedClassName}
import miksilo.modularLanguages.deltas.javac.methods.{AccessibilityFieldsDelta, MethodParameters}
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.statement.BlockDelta
import miksilo.modularLanguages.util.{JavaSourceUtils, TestLanguageBuilder}
import org.scalatest.funsuite.AnyFunSuite

class EmptyMain extends AnyFunSuite {
  val className = "EmptyMain"
  val defaultPackage = Seq()
  val other = new FibonacciWithoutMain()

  test("runCompiledCode") {
    val byteCode: Node = getByteCode
    JavaSourceUtils.runByteCode(className, byteCode)
  }

  def getByteCode: Node = {
    val java = getJava
    val byteCode = TestLanguageBuilder.build(JavaToByteCodeLanguage.javaCompilerDeltas).compileAst(java).program
    byteCode.asInstanceOf[PathRoot].current
  }

  def getJava: Node = {
    JavaClassDelta.neww(defaultPackage, className, Seq(getMainMethodJava))
  }

  def getMainMethodJava: Node = {
    val parameters = Seq(MethodParameters.neww("args", ArrayTypeDelta.neww(QualifiedObjectTypeDelta.neww(QualifiedClassName(Seq("java", "lang", "String"))))))
    MethodDelta.neww("main", VoidTypeDelta.voidType, parameters,  BlockDelta.neww(), static = true, AccessibilityFieldsDelta.PublicVisibility)
  }
}
