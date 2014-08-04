package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.base.ConstantPool

object GotoC extends InstructionC {

  override val key: Any = GoToKey

  def goTo(target: Int): MetaObject = ByteCodeSkeleton.instruction(GoToKey, Seq(target))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = 0

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("a7") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(), Seq())

  override def getInstructionSize: Int = 3

  object GoToKey

}
