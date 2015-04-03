package transformations.javac

import core.particles.node.Node
import org.junit.Test
import transformations.javac.classes.JavaClassSkeleton._
import transformations.javac.classes.QualifiedClassName
import transformations.javac.methods.MethodC._
import transformations.bytecode.types.{ArrayTypeC, ObjectTypeC, VoidTypeC}
import util.TestUtils

class EmptyMain {
  val className = "EmptyMain"
  val defaultPackage = Seq()
  val other = new FibonacciWithoutMain()

  @Test
  def runCompiledCode() {
    val byteCode: Node = getByteCode
    TestUtils.runByteCode(className, byteCode)
  }


  def getByteCode: Node = {
    val java = getJava
    val byteCode = JavaCompiler.getCompiler.transform(java)
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
