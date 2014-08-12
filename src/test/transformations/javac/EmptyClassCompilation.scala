package transformations.javac

import core.transformation.MetaObject
import org.junit.Test
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.coreInstructions.{InvokeSpecialC, LoadAddressC, VoidReturnInstructionC}
import transformations.javac.classes.{ClassC, QualifiedClassName}
import transformations.types.VoidTypeC

import scala.collection.mutable.ArrayBuffer

class EmptyClassCompilation {
  val className: String = "EmptyClass"
  val classPackage: Seq[String] = Seq("transformations", "java", "testing")

  @Test
  def testEquivalentConstantPool() {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode: MetaObject = getEmptyClass
    val compiledCode = JavaCompiler.getTransformer.transform(javaCode)
    TestUtils.compareConstantPools(expectedByteCode, compiledCode)
  }

  @Test
  def testEquivalentMethod() {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode = getEmptyClass
    val compiledCode = JavaCompiler.getTransformer.transform(javaCode)

    TestUtils.testInstructionEquivalence(expectedByteCode, compiledCode)
  }

  def getEmptyClassByteCode = {
    val constantPool = ArrayBuffer[Any](ByteCodeSkeleton.methodRef(3, 10),
      ByteCodeSkeleton.classRef(11),
      ByteCodeSkeleton.classRef(12),
      ConstructorC.constructorName,
      ByteCodeSkeleton.methodDescriptor(VoidTypeC.voidType, Seq()),
      ByteCodeSkeleton.CodeAttributeId,
      ByteCodeSkeleton.nameAndType(4, 5),
      new QualifiedClassName(Seq("transformations", "java", "testing", "EmptyClass")),
      new QualifiedClassName(Seq("java", "lang", "Object"))
    )
    val instructions = Seq(LoadAddressC.addressLoad(0), InvokeSpecialC.invokeSpecial(1), VoidReturnInstructionC.voidReturn)
    val codeAttribute = Seq(ByteCodeSkeleton.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    val defaultConstructor = ByteCodeSkeleton.methodInfo(3, 4, codeAttribute, Set(ByteCodeSkeleton.PublicAccess))
    ByteCodeSkeleton.clazz(2, 3, constantPool, Seq(defaultConstructor))
  }

  def getEmptyClass: MetaObject = {
    ClassC.clazz(classPackage, className, methods = Seq[MetaObject]())
  }
}
