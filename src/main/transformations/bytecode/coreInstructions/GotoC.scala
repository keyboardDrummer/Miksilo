package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.classes.ConstantPool

object GotoC extends InstructionC {

  override val key: AnyRef = GoToKey

  def goTo(target: Int): MetaObject = ByteCodeSkeleton.instruction(GoToKey, Seq(target))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = 0

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("a7") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(false, true)

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState) = (Seq(), Seq())

  override def getInstructionSize(instruction: MetaObject): Int = 3

  object GoToKey

}
