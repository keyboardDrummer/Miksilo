package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

object StoreAddressC extends InstructionC {
  override val key: Any = AddressStore

  def addressStore(location: Int): MetaObject = instruction(AddressStore, Seq(location))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = -1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("3a") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("4b") + location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(JavaTypes.intType), Seq())

  override def getInstructionSize: Int = 2

  object AddressStore

}
