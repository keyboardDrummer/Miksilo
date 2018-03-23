package deltas.javac

import core.language.node.Node
import deltas.bytecode.types.{ArrayTypeDelta, QualifiedObjectTypeDelta, VoidTypeDelta}
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton.QualifiedClassName
import deltas.javac.methods.AccessibilityFieldsDelta
import deltas.javac.methods.MethodDelta._
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
    val byteCode = TestLanguageBuilder.build(JavaLanguage.javaCompilerDeltas).transform(java).program
    byteCode
  }

  def getJava: Node = {
    neww(defaultPackage, className, Seq(getMainMethodJava))
  }

  def getMainMethodJava: Node = {
    val parameters = Seq(parameter("args", ArrayTypeDelta.arrayType(QualifiedObjectTypeDelta.neww(QualifiedClassName(Seq("java", "lang", "String"))))))
    val body = Seq()
    method("main", VoidTypeDelta.voidType, parameters, body, static = true, AccessibilityFieldsDelta.PublicVisibility)
  }
}
