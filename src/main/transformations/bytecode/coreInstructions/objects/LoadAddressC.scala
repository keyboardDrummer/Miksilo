package transformations.bytecode.coreInstructions.objects

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.InstructionC
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object LoadAddressC extends InstructionC {

  override val key: AnyRef = AddressLoad

  def addressLoad(location: Int): MetaObject = ByteCodeSkeleton.instruction(AddressLoad, Seq(location))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      PrintByteCode.hexToBytes("19") ++ PrintByteCode.byteToBytes(location)
    else
      PrintByteCode.byteToBytes(PrintByteCode.hexToInt("2a") + location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState) = (Seq(), Seq(IntTypeC.intType))

  object AddressLoad

}
