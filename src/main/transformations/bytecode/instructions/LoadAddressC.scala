package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

object LoadAddressC extends InstructionC {

  override val key: Any = AddressLoad

  def addressLoad(location: Int): MetaObject = ByteCodeSkeleton.instruction(AddressLoad, Seq(location))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      PrintByteCode.hexToBytes("19") ++ PrintByteCode.byteToBytes(location)
    else
      PrintByteCode.byteToBytes(PrintByteCode.hexToInt("2a") + location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(), Seq(JavaTypes.intType))

  override def getInstructionSize: Int = 1

  object AddressLoad

}
