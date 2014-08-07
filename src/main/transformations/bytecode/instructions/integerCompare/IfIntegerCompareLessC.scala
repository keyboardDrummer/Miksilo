package transformations.bytecode.instructions.integerCompare

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.instructions.InstructionC
import transformations.javac.base.ConstantPool
import transformations.javac.types.IntTypeC

object IfIntegerCompareLessC extends InstructionC {

  override val key: AnyRef = IfIntegerCompareLessKey

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction(IfIntegerCompareLessKey, Seq(target))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = -2

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("a4") ++ shortToBytes(arguments(0))
  }

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(true, true)

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject]) =
    (Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  override def getInstructionSize: Int = 3

  object IfIntegerCompareLessKey

}
