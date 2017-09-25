package transformations.javac

import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.types.{ArrayTypeC, ObjectTypeC, VoidTypeC}
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.classes.skeleton.QualifiedClassName
import transformations.javac.methods.MethodC._
import util.{CompilerBuilder, TestUtils}

class EmptyMain extends FunSuite {
  val className = "EmptyMain"
  val defaultPackage = Seq()
  val other = new FibonacciWithoutMain()

  test("runCompiledCode") {
    val byteCode: Node = getByteCode
    TestUtils.runByteCode(className, byteCode)
  }

  def getByteCode: Node = {
    val java = getJava
    val byteCode = CompilerBuilder.build(JavaCompiler.javaCompilerTransformations).transform(java).program
    byteCode
  }

  def getJava: Node = {
    clazz(defaultPackage, className, Seq(getMainMethodJava))
  }

  def getMainMethodJava: Node = {
    val parameters = Seq(parameter("args", ArrayTypeC.arrayType(ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String"))))))
    val body = Seq()
    method("main", VoidTypeC.voidType, parameters, body, static = true, PublicVisibility)
  }
}
