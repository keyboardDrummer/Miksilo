package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.base.ConstantPool

object VoidReturnC extends InstructionC {

  override val key: Any = VoidReturn

  def voidReturn: MetaObject = ByteCodeSkeleton.instruction(VoidReturn)

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = 0

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("b1")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(), Seq())

  override def getInstructionSize: Int = 1

  object VoidReturn

}
