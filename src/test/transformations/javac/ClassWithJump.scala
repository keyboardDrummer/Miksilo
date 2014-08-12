package transformations.javac

import core.transformation.MetaObject
import org.junit.Test
import transformations.javac.classes.ClassC._
import transformations.javac.classes.QualifiedClassName
import transformations.javac.expressions.{BooleanLiteralC, NumberLiteralC, TernaryC}
import transformations.javac.methods.MethodC._
import transformations.javac.methods._
import transformations.javac.types._

class ClassWithJump {

  val className = "ClassWithJump"
  val defaultPackage = Seq()
  val other = new FibonacciWithoutMain()

  @Test
  def runCompiledCode() {
    val byteCode: MetaObject = getByteCode
    TestUtils.runByteCode(className, byteCode)
  }

  def getByteCode: MetaObject = {
    val java = getJava
    val byteCode = JavaCompiler.getTransformer.transform(java)
    byteCode
  }

  def getJava: MetaObject = {
    clazz(defaultPackage, className, Seq(getTestMethod, getMainMethodJava))
  }

  def getMainMethodJava: MetaObject = {
    val parameters = Seq(parameter("args", ArrayTypeC.arrayType(ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String"))))))
    val fibCall = CallC.call(VariableC.variable("test"), Seq(BooleanLiteralC.literal(true)))
    val body = Seq[MetaObject](CallC.call(SelectorC.selector(SelectorC.selector(SelectorC.selector(SelectorC.selector(
      VariableC.variable("java"), "lang"), "System"), "out"), "print"), Seq(fibCall)))
    method("main", VoidTypeC.voidType, parameters, body, static = true, PublicVisibility)
  }

  def getTestMethod = {
    val parameters = Seq(parameter("b", BooleanTypeC.booleanType))
    val body = Seq(ReturnExpressionC._return(TernaryC.ternary(VariableC.variable("b"), NumberLiteralC.literal(3), NumberLiteralC.literal(4))))
    method("test", IntTypeC.intType, parameters, body, static = true, PrivateVisibility)
  }
}
