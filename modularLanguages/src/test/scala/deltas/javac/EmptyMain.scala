package deltas.javac

import core.deltas.path.PathRoot
import core.language.node.Node
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, VoidTypeDelta}
import deltas.javac.classes.skeleton.{JavaClassDelta, QualifiedClassName}
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta, MethodParameters}
import deltas.statement.BlockDelta
import org.scalatest.funsuite.AnyFunSuite
import util.{JavaSourceUtils, TestLanguageBuilder}

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
