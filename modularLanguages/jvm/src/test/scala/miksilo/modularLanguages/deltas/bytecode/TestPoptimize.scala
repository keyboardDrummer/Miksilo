package miksilo.modularLanguages.deltas.bytecode

import miksilo.modularLanguages.core.node.{Node, NodeWrapper}
import miksilo.modularLanguages.deltas.bytecode.additions.PoptimizeDelta
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.constants.Utf8ConstantDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.{SmallIntegerConstantDelta, StoreIntegerDelta}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs.PushLongDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{Pop2Delta, PopDelta, VoidReturnInstructionDelta}
import miksilo.modularLanguages.deltas.bytecode.extraConstants.TypeConstant
import miksilo.modularLanguages.deltas.bytecode.types.VoidTypeDelta
import miksilo.modularLanguages.deltas.javac.classes.ConstantPool
import miksilo.modularLanguages.deltas.javac.types.MethodTypeDelta
import org.scalatest.funsuite.AnyFunSuite
import util.TestLanguageBuilder

class TestPoptimize extends AnyFunSuite {

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

  def transformInstructions(instructions: Seq[Node]): Seq[Node] = {
    val codeAnnotation = CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq())
    val method = ByteCodeMethodInfo.Shape.create(
      ByteCodeMethodInfo.MethodNameIndex -> Utf8ConstantDelta.create("name"),
      ByteCodeMethodInfo.MethodDescriptor -> TypeConstant.constructor(MethodTypeDelta.neww(VoidTypeDelta.voidType,Seq.empty)),
      ByteCodeMethodInfo.MethodAttributes -> Seq(codeAnnotation.node))

    method(ByteCodeMethodInfo.AccessFlagsKey) = Set(ByteCodeMethodInfo.StaticAccess)
    val shape = ByteCodeSkeleton.neww(0, 0, new ConstantPool(Seq()), Seq(method))
    val compiler = TestLanguageBuilder.build(Seq(PoptimizeDelta) ++ ByteCodeLanguage.byteCodeDeltas)
    compiler.compileAst(shape)
    NodeWrapper.unwrapList(codeAnnotation.instructions)
  }

  test("Pop2") {
    val instructions = Seq(PushLongDelta.constant(1), Pop2Delta.pop2, VoidReturnInstructionDelta.voidReturn)
    val expected = Seq(VoidReturnInstructionDelta.voidReturn)
    val newInstructions = transformInstructions(instructions)
    assertResult(expected)(newInstructions)
  }
}
