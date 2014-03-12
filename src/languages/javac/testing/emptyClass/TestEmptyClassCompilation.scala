package languages.javac.testing.emptyClass

import languages.bytecode.ByteCode
import languages.javac.base.{QualifiedClassName, JavaClassModel, JavaTypes}
import org.junit.Test
import languages.javac.{ConstructorC, JavaCompiler}
import transformation.MetaObject
import scala.collection.mutable.ArrayBuffer
import languages.javac.testing.TestUtils

class TestEmptyClassCompilation {
  val classname: String = "EmptyClass"

  def getEmptyClassByteCode() = {
    val constantPool = ArrayBuffer[Any](ByteCode.methodRef(3, 10),
      ByteCode.classRef(11),
      ByteCode.classRef(12),
      ConstructorC.constructorName,
      ByteCode.methodDescriptor(JavaTypes.VoidType, Seq()),
      ByteCode.CodeAttributeId,
      ByteCode.nameAndType(4, 5),
      new QualifiedClassName(Seq("languages","java","testing","EmptyClass")),
      new QualifiedClassName(Seq("java","lang","Object"))
    )
    val instructions = Seq(ByteCode.addressLoad(0), ByteCode.invokeSpecial(1), ByteCode.voidReturn)
    val codeAttribute = Seq(ByteCode.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    val defaultConstructor = ByteCode.methodInfo(3,4, codeAttribute, Set(ByteCode.PublicAccess))
    ByteCode.clazz(2, 3, constantPool, Seq(defaultConstructor))
  }

  val classPackage: Seq[String] = Seq("languages","java","testing")

  def getEmptyClass() = {
    JavaClassModel.clazz(classPackage, classname, methods = Seq[MetaObject]())

  }

  @Test
  def testEquivalentConstantPool() {
    val expectedByteCode = getEmptyClassByteCode()
    val javaCode = getEmptyClass()
    val compiledCode = JavaCompiler.getCompiler.compile(javaCode)
    TestUtils.compareConstantPools(expectedByteCode, compiledCode)
  }

  @Test
  def testEquivalentMethod() {
    val expectedByteCode = getEmptyClassByteCode()
    val javaCode = getEmptyClass()
    val compiledCode = JavaCompiler.getCompiler.compile(javaCode)

    TestUtils.testInstructionEquivalence(expectedByteCode, compiledCode)
  }
}
