package transformations.bytecode.coreInstructions.objects

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object StoreAddressC extends InstructionC {
  override val key: AnyRef = AddressStore

  def addressStore(location: Int): MetaObject = instruction(AddressStore, Seq(location))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("3a") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("4b") + location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, stackTypes: Seq[MetaObject],
                                          state: TransformationState): InstructionSignature = InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def getVariableUpdates(instruction: MetaObject): Map[Int, MetaObject] =
    Map(getInstructionArguments(instruction)(0) -> IntTypeC.intType)

  object AddressStore

}
