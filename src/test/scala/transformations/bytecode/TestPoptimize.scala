package transformations.bytecode

import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.additions.PoptimizeC
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.Utf8ConstantDelta
import transformations.bytecode.coreInstructions.integers.{SmallIntegerConstantDelta, StoreIntegerDelta}
import transformations.bytecode.coreInstructions.longs.PushLongDelta
import transformations.bytecode.coreInstructions.{Pop2Delta, PopDelta, VoidReturnInstructionDelta}
import transformations.bytecode.extraConstants.TypeConstant
import transformations.bytecode.types.VoidTypeC
import transformations.javac.JavaCompilerDeltas
import transformations.javac.classes.ConstantPool
import transformations.javac.types.MethodType
import util.CompilerBuilder

class TestPoptimize extends FunSuite {

  test("Basic") {
    val instructions = Seq(SmallIntegerConstantDelta.integerConstant(3), PopDelta.pop, VoidReturnInstructionDelta.voidReturn)
    val newInstructions = transformInstructions(instructions)
    assertResult(Seq(VoidReturnInstructionDelta.voidReturn))(newInstructions)
  }

  test("Memory") {
    val instructions = Seq(SmallIntegerConstantDelta.integerConstant(3),
      SmallIntegerConstantDelta.integerConstant(2),
      PopDelta.pop,
      PopDelta.pop,
      VoidReturnInstructionDelta.voidReturn)
    val newInstructions = transformInstructions(instructions)
    assertResult(Seq(VoidReturnInstructionDelta.voidReturn))(newInstructions)
  }

  test("Encapsulation") {
    val middle = Seq(SmallIntegerConstantDelta.integerConstant(2), StoreIntegerDelta.integerStore(0))
    val expected = middle ++ Seq(VoidReturnInstructionDelta.voidReturn)
    val instructions = Seq(SmallIntegerConstantDelta.integerConstant(3)) ++ middle ++ Seq(PopDelta.pop, VoidReturnInstructionDelta.voidReturn)
    val newInstructions = transformInstructions(instructions)
    assertResult(expected)(newInstructions)
  }

  test("Advanced") {
    val expected = Seq(SmallIntegerConstantDelta.integerConstant(3),
      SmallIntegerConstantDelta.integerConstant(3),
      StoreIntegerDelta.integerStore(0),
      StoreIntegerDelta.integerStore(0),
      VoidReturnInstructionDelta.voidReturn)
    val instructions = Seq(SmallIntegerConstantDelta.integerConstant(3),
      SmallIntegerConstantDelta.integerConstant(3),
      SmallIntegerConstantDelta.integerConstant(3),
      PopDelta.pop,
      SmallIntegerConstantDelta.integerConstant(3),
      SmallIntegerConstantDelta.integerConstant(3),
      StoreIntegerDelta.integerStore(0),
      PopDelta.pop,
      StoreIntegerDelta.integerStore(0),
      PopDelta.pop,
      VoidReturnInstructionDelta.voidReturn)
    val newInstructions = transformInstructions(instructions)
    assertResult(expected)(newInstructions)
  }

  test("Robustness"){
    val instructions = Seq(SmallIntegerConstantDelta.integerConstant(3), VoidReturnInstructionDelta.voidReturn)
    val newInstructions = transformInstructions(instructions)
    assertResult(instructions)(newInstructions)
  }

  def transformInstructions(instructions: Seq[Node]) = {
    val codeAnnotation = CodeAttribute.codeAttribute(0, 0, 0, instructions, Seq(), Seq())
    val method = ByteCodeMethodInfo.MethodInfoKey.create(
      ByteCodeMethodInfo.MethodNameIndex -> Utf8ConstantDelta.create("name"),
      ByteCodeMethodInfo.MethodDescriptor -> TypeConstant.constructor(MethodType.construct(VoidTypeC.voidType,Seq.empty)),
      ByteCodeMethodInfo.MethodAttributes -> Seq(codeAnnotation.node))

    method(ByteCodeMethodInfo.AccessFlagsKey) = Set(ByteCodeMethodInfo.StaticAccess)
    val clazz = ByteCodeSkeleton.clazz(0, 0, new ConstantPool(Seq()), Seq(method))
    val compiler = CompilerBuilder.build(Seq(PoptimizeC) ++ JavaCompilerDeltas.byteCodeTransformations)
    compiler.transform(clazz)
    codeAnnotation.instructions
  }

  test("Pop2") {
    val instructions = Seq(PushLongDelta.constant(1), Pop2Delta.pop2, VoidReturnInstructionDelta.voidReturn)
    val expected = Seq(VoidReturnInstructionDelta.voidReturn)
    val newInstructions = transformInstructions(instructions)
    assertResult(expected)(newInstructions)
  }
}
