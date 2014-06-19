package languages.javac.testing.classWithEmptyMain

import org.junit.{Assert, Test}
import languages.javac.{LiteralC, ConstructorC, JavaCompiler}
import languages.bytecode._
import transformation.{TransformationState, ProgramTransformation, TransformationManager, MetaObject}
import languages.javac.base.model._
import JavaClassModel._
import languages.javac.base.model.QualifiedClassName
import JavaMethodModel._
import JavaTypes._
import JavaBaseModel._
import scala.collection.mutable.ArrayBuffer
import languages.javac.testing.TestUtils
import languages.javac.testing.fibonacciWithoutMain.TestFibonacciCompilation

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
