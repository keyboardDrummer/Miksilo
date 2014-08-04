package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

object PushNullC extends InstructionC {

  override val key: Any = PushNullKey
  val pushNull = ByteCodeSkeleton.instruction(PushNullC)

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("01")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(), Seq(JavaTypes.intType))

  override def getInstructionSize: Int = 1

  object PushNullKey

}
