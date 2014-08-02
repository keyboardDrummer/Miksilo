package transformations.javac

import core.transformation.MetaObject
import org.junit.Test
import transformations.bytecode.ByteCode
import transformations.javac.base.model.{JavaClassModel, JavaTypes, QualifiedClassName}

import scala.collection.mutable.ArrayBuffer

class EmptyClassCompilation {
  val className: String = "EmptyClass"
  val classPackage: Seq[String] = Seq("transformations", "java", "testing")

  @Test
  def testEquivalentConstantPool() {
    val expectedByteCode = getEmptyClassByteCode()
    val javaCode = getEmptyClass()
    val compiledCode = JavaCompiler.getCompiler.transform(javaCode)
    TestUtils.compareConstantPools(expectedByteCode, compiledCode)
  }

  @Test
  def testEquivalentMethod() {
    val expectedByteCode = getEmptyClassByteCode()
    val javaCode = getEmptyClass()
    val compiledCode = JavaCompiler.getCompiler.transform(javaCode)

    TestUtils.testInstructionEquivalence(expectedByteCode, compiledCode)
  }

  def getEmptyClassByteCode() = {
    val constantPool = ArrayBuffer[Any](ByteCode.methodRef(3, 10),
      ByteCode.classRef(11),
      ByteCode.classRef(12),
      ConstructorC.constructorName,
      ByteCode.methodDescriptor(JavaTypes.voidType, Seq()),
      ByteCode.CodeAttributeId,
      ByteCode.nameAndType(4, 5),
      new QualifiedClassName(Seq("transformations", "java", "testing", "EmptyClass")),
      new QualifiedClassName(Seq("java", "lang", "Object"))
    )
    val instructions = Seq(ByteCode.addressLoad(0), ByteCode.invokeSpecial(1), ByteCode.voidReturn)
    val codeAttribute = Seq(ByteCode.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    val defaultConstructor = ByteCode.methodInfo(3, 4, codeAttribute, Set(ByteCode.PublicAccess))
    ByteCode.clazz(2, 3, constantPool, Seq(defaultConstructor))
  }

  def getEmptyClass() = {
    JavaClassModel.clazz(classPackage, className, methods = Seq[MetaObject]())

  }
}
