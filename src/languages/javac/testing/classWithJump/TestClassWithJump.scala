package languages.javac.testing.classWithJump

import languages.javac.testing.fibonacciWithoutMain.TestFibonacciCompilation
import org.junit.Test
import transformation.MetaObject
import languages.javac.testing.TestUtils
import languages.javac.{LiteralC, TernaryC, JavaCompiler}
import languages.javac.base.JavaMethodModel._
import languages.javac.base.JavaTypes._
import languages.javac.base._
import languages.javac.base.JavaClassModel._
import languages.javac.base.JavaBaseModel._
import languages.javac.base.QualifiedClassName
import scala.Some

class TestClassWithJump {

  val className = "ClassWithJump"
  val defaultPackage = Seq()
  val other = new TestFibonacciCompilation()

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
    clazz(defaultPackage, className, Seq(getMainMethodJava, getTestMethod))
  }
}
