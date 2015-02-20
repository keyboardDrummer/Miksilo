package transformations.bytecode.coreInstructions.longs

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.javac.classes.ConstantPool
import transformations.types.LongTypeC

object LoadLongC extends InstructionC {

  override val key: AnyRef = LongLoad

  def load(location: Integer) = ByteCodeSkeleton.instruction(LongLoad, Seq(location))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("16") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("1e") + location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, stackTypes: Seq[MetaObject],
                                          state: TransformationState): InstructionSignature =
    InstructionSignature(Seq(), Seq(LongTypeC.longType))

  object LongLoad
}

