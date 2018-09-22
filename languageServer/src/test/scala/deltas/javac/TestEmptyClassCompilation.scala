package deltas.javac

import core.language.node.Node
import org.scalatest.FunSuite
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.constants._
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.coreInstructions.{InvokeSpecialDelta, VoidReturnInstructionDelta}
import deltas.bytecode.extraConstants.{QualifiedClassNameConstantDelta, TypeConstant}
import deltas.bytecode.types.VoidTypeDelta
import deltas.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import deltas.javac.classes.ConstantPool
import deltas.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}
import deltas.javac.constructor.SuperCallExpression
import deltas.javac.types.MethodType
import util.{TestLanguageBuilder, LanguageTest}

class TestEmptyClassCompilation extends FunSuite {
  val className: String = "EmptyClass"
  val classPackage: Seq[String] = Seq("transformations", "java", "testing")

  test("EquivalentConstantPool") {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode: Node = getEmptyClass
    val compiledCode = TestLanguageBuilder.build(JavaLanguage.javaCompilerDeltas).transform(javaCode).program
    LanguageTest.compareConstantPools(expectedByteCode, compiledCode)
  }

  test("EquivalentMethod") {
    val expectedByteCode = getEmptyClassByteCode
    val javaCode = getEmptyClass
    val compiledCode = TestLanguageBuilder.build(JavaLanguage.javaCompilerDeltas).transform(javaCode).program

    LanguageTest.testInstructionEquivalence(expectedByteCode, compiledCode)
  }

  def getEmptyClassByteCode: Node = {
    val constantPool = new ConstantPool(Seq(MethodRefConstant.methodRef(3, 10),
      ClassInfoConstant.classRef(11),
      ClassInfoConstant.classRef(12),
      Utf8ConstantDelta.create(SuperCallExpression.constructorName),
      TypeConstant.constructor(MethodType.construct(VoidTypeDelta.voidType, Seq())),
      CodeAttributeDelta.constantEntry,
      NameAndTypeConstant.nameAndType(4, 5),
      QualifiedClassNameConstantDelta.create(QualifiedClassName(Seq("transformations", "java", "testing", "EmptyClass"))),
      QualifiedClassNameConstantDelta.create(QualifiedClassName(Seq("java", "lang", "Object"))))
    )
    val instructions = Seq(LoadAddressDelta.addressLoad(0), InvokeSpecialDelta.invokeSpecial(1), VoidReturnInstructionDelta.voidReturn)
    val codeAttribute = Seq(CodeAttributeDelta.codeAttribute(5, 1, 1, instructions, Seq(), Seq()))
    val defaultConstructor = ByteCodeMethodInfo.methodInfo(3, 4, codeAttribute, Set(ByteCodeMethodInfo.PublicAccess))
    ByteCodeSkeleton.neww(2, 3, constantPool, Seq(defaultConstructor))
  }

  def getEmptyClass: Node = {
    JavaClassSkeleton.neww(classPackage, className, members = Seq[Node]())
  }
}
