package transformations.javac

import core.transformation.MetaObject
import org.junit.Test
import transformations.javac.base.model.JavaClassModel._
import transformations.javac.base.model.JavaMethodModel._
import transformations.javac.base.model.JavaTypes._
import transformations.javac.base.model._
import transformations.javac.expressions.{LiteralC, TernaryC}
import transformations.javac.methods.{CallC, ReturnC, SelectorC, VariableC}

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

  def getJava: MetaObject = {
    clazz(defaultPackage, className, Seq(getTestMethod, getMainMethodJava))
  }

  def getMainMethodJava: MetaObject = {
    val parameters = Seq(parameter("args", arrayType(objectType(new QualifiedClassName(Seq("java", "lang", "String"))))))
    val fibCall = CallC.call(VariableC.variable("test"), Seq(LiteralC.literal(true)))
    val body = Seq[MetaObject](CallC.call(SelectorC.selector(SelectorC.selector(SelectorC.selector(SelectorC.selector(
      VariableC.variable("java"), "lang"), "System"), "out"), "print"), Seq(fibCall)))
    method("main", JavaTypes.voidType, parameters, body, static = true, PublicVisibility)
  }

  def getTestMethod = {
    val parameters = Seq(parameter("b", JavaTypes.booleanType))
    val body = Seq(ReturnC._return(Some(TernaryC.ternary(VariableC.variable("b"), LiteralC.literal(3), LiteralC.literal(4)))))
    method("test", JavaTypes.intType, parameters, body, static = true, PrivateVisibility)
  }
}
