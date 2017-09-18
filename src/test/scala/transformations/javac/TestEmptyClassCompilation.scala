package transformations.javac

import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.attributes.{CodeAttribute, CodeConstantEntry}
import transformations.bytecode.constants._
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.bytecode.coreInstructions.{InvokeSpecialC, VoidReturnInstructionC}
import transformations.bytecode.extraConstants.{QualifiedClassNameConstant, TypeConstant}
import transformations.bytecode.types.VoidTypeC
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool
import transformations.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}
import transformations.javac.constructor.SuperCallExpression
import transformations.javac.types.MethodType
import util.TestUtils

class TestEmptyClassCompilation extends FunSuite {
  val className: String = "EmptyClass"
  val classPackage: Seq[String] = Seq("transformations", "java", "testing")

  test("EquivalentConstantPool") {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode: Node = getEmptyClass
    val compiledCode = JavaCompiler.getCompiler.transform(javaCode)
    TestUtils.compareConstantPools(expectedByteCode, compiledCode)
  }

  test("EquivalentMethod") {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode = getEmptyClass
    val compiledCode = JavaCompiler.getCompiler.transform(javaCode)

    TestUtils.testInstructionEquivalence(expectedByteCode, compiledCode)
  }

  def getEmptyClassByteCode: Node = {
    val constantPool = new ConstantPool(Seq(MethodRefConstant.methodRef(3, 10),
      ClassInfoConstant.classRef(11),
      ClassInfoConstant.classRef(12),
      Utf8Constant.create(SuperCallExpression.constructorName),
      TypeConstant.constructor(MethodType.construct(VoidTypeC.voidType, Seq())),
      CodeConstantEntry.entry,
      NameAndTypeConstant.nameAndType(4, 5),
      QualifiedClassNameConstant.create(QualifiedClassName(Seq("transformations", "java", "testing", "EmptyClass"))),
      QualifiedClassNameConstant.create(QualifiedClassName(Seq("java", "lang", "Object"))))
    )
    val instructions = Seq(LoadAddressC.addressLoad(0), InvokeSpecialC.invokeSpecial(1), VoidReturnInstructionC.voidReturn)
    val codeAttribute = Seq(CodeAttribute.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    val defaultConstructor = ByteCodeMethodInfo.methodInfo(3, 4, codeAttribute, Set(ByteCodeMethodInfo.PublicAccess))
    ByteCodeSkeleton.clazz(2, 3, constantPool, Seq(defaultConstructor))
  }

  def getEmptyClass: Node = {
    JavaClassSkeleton.clazz(classPackage, className, members = Seq[Node]())
  }
}
