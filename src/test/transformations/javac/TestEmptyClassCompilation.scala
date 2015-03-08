package transformations.javac

import core.transformation.MetaObject
import org.junit.Test
import transformations.bytecode.attributes.{CodeAttribute, CodeConstantEntry}
import transformations.bytecode.constants.{ClassRefConstant, MethodDescriptorConstant, MethodRefConstant, NameAndType}
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.bytecode.coreInstructions.{InvokeSpecialC, VoidReturnInstructionC}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.javac.classes.{JavaClassSkeleton, ConstantPool, QualifiedClassName}
import transformations.javac.constructor.SuperCallExpression
import transformations.types.VoidTypeC
import util.TestUtils

class TestEmptyClassCompilation {
  val className: String = "EmptyClass"
  val classPackage: Seq[String] = Seq("transformations", "java", "testing")

  @Test
  def testEquivalentConstantPool() {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode: MetaObject = getEmptyClass
    val compiledCode = JavaCompiler.getCompiler.transform(javaCode)
    TestUtils.compareConstantPools(expectedByteCode, compiledCode)
  }

  @Test
  def testEquivalentMethod() {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode = getEmptyClass
    val compiledCode = JavaCompiler.getCompiler.transform(javaCode)

    TestUtils.testInstructionEquivalence(expectedByteCode, compiledCode)
  }

  def getEmptyClassByteCode = {
    val constantPool = new ConstantPool(Seq(MethodRefConstant.methodRef(3, 10),
      ClassRefConstant.classRef(11),
      ClassRefConstant.classRef(12),
      SuperCallExpression.constructorName,
      MethodDescriptorConstant.methodDescriptor(VoidTypeC.voidType, Seq()),
      CodeConstantEntry.entry,
      NameAndType.nameAndType(4, 5),
      new QualifiedClassName(Seq("transformations", "java", "testing", "EmptyClass")),
      new QualifiedClassName(Seq("java", "lang", "Object")))
    )
    val instructions = Seq(LoadAddressC.addressLoad(0), InvokeSpecialC.invokeSpecial(1), VoidReturnInstructionC.voidReturn)
    val codeAttribute = Seq(CodeAttribute.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    val defaultConstructor = ByteCodeMethodInfo.methodInfo(3, 4, codeAttribute, Set(ByteCodeMethodInfo.PublicAccess))
    ByteCodeSkeleton.clazz(2, 3, constantPool, Seq(defaultConstructor))
  }

  def getEmptyClass: MetaObject = {
    JavaClassSkeleton.clazz(classPackage, className, members = Seq[MetaObject]())
  }
}
