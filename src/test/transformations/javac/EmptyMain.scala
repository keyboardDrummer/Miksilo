package transformations.javac

import core.particles.MetaObject
import org.junit.Test
import transformations.javac.classes.JavaClassSkeleton._
import transformations.javac.classes.QualifiedClassName
import transformations.javac.methods.MethodC._
import transformations.types.{ArrayTypeC, ObjectTypeC, VoidTypeC}
import util.TestUtils

class EmptyMain {
  val className = "EmptyMain"
  val defaultPackage = Seq()
  val other = new FibonacciWithoutMain()

  @Test
  def runCompiledCode() {
    val byteCode: MetaObject = getByteCode
    TestUtils.runByteCode(className, byteCode)
  }


  def getByteCode: MetaObject = {
    val java = getJava
    val byteCode = JavaCompiler.getCompiler.transform(java)
    byteCode
  }

  def getJava: MetaObject = {
    clazz(defaultPackage, className, Seq(getMainMethodJava))
  }

  def getMainMethodJava: MetaObject = {
    val parameters = Seq(parameter("args", ArrayTypeC.arrayType(ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String"))))))
    val body = Seq()
    method("main", VoidTypeC.voidType, parameters, body, static = true, PublicVisibility)
  }
}
