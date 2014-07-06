package transformations.javac.classWithEmptyMain

import org.junit.{Assert, Test}
import transformations.javac.{TestUtils, LiteralC, ConstructorC, JavaCompiler}
import transformations.bytecode._
import core.transformation.{TransformationState, ProgramTransformation, TransformationManager, MetaObject}
import transformations.javac.base.model._
import JavaClassModel._
import transformations.javac.base.model.QualifiedClassName
import JavaMethodModel._
import JavaTypes._
import JavaBaseModel._
import scala.collection.mutable.ArrayBuffer
import transformations.javac.fibonacciWithoutMain.TestFibonacciCompilation

class TestEmptyMain {
  val className = "EmptyMain"
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
    val body = Seq()
    method("main", VoidType, parameters, body, static = true, PublicVisibility)
  }

  def getJava: MetaObject = {
    clazz(defaultPackage, className, Seq(getMainMethodJava))
  }
}
