package transformations.javac

import org.junit.Test
import core.transformation.MetaObject
import transformations.javac.{FibonacciWthoutMain, JavaCompiler}
import transformations.javac.base.model._
import JavaMethodModel._
import transformations.javac.base.model.{JavaTypes, JavaBaseModel, JavaClassModel, QualifiedClassName}
import JavaTypes._
import JavaClassModel._
import JavaBaseModel._
import scala.Some

class ClassWithJump {

  val className = "ClassWithJump"
  val defaultPackage = Seq()
  val other = new FibonacciWthoutMain()

  @Test
  def runCompiledCode() {
    val byteCode: MetaObject = getByteCode
    TestUtils.runByteCode(className, byteCode)
  }

  def getByteCode: MetaObject = {
    val java = getJava
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(java)
    byteCode
  }

  def getMainMethodJava: MetaObject = {
    val parameters = Seq(parameter("args", arrayType(objectType(new QualifiedClassName(Seq("java", "lang", "String"))))))
    val fibCall = call(variable("test"), Seq(LiteralC.literal(true)))
    val body = Seq(call(selector(selector(selector(selector(variable("java"), "lang"), "System"), "out"), "print"), Seq(fibCall)))
    method("main", VoidType, parameters, body, static = true, PublicVisibility)
  }

  def getTestMethod = {
    val parameters = Seq(parameter("b", JavaTypes.BooleanType))
    val body = Seq(_return(Some(TernaryC.ternary(JavaBaseModel.variable("b"), LiteralC.literal(3), LiteralC.literal(4)))))
    method("test", IntegerType, parameters, body, static = true, PrivateVisibility)
  }

  def getJava: MetaObject = {
    clazz(defaultPackage, className, Seq(getTestMethod, getMainMethodJava))
  }
}
