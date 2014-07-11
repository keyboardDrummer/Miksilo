package transformations.javac

import core.transformation.MetaObject
import org.junit.Test
import transformations.javac.base.model.JavaBaseModel._
import transformations.javac.base.model.JavaClassModel._
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model.JavaTypes._
import transformations.javac.base.model._
import transformations.javac.expressions.{LiteralC, TernaryC}

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
    val byteCode = compiler.transform(java)
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
    method("test", IntType, parameters, body, static = true, PrivateVisibility)
  }

  def getJava: MetaObject = {
    clazz(defaultPackage, className, Seq(getTestMethod, getMainMethodJava))
  }
}
