package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.base.ConstantPool
import transformations.javac.types.IntTypeC

object PushNullC extends InstructionC {

  override val key: AnyRef = PushNullKey
  val pushNull = ByteCodeSkeleton.instruction(PushNullC)

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("01")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize: Int = 1

  object PushNullKey

}
