package transformations.bytecode.coreInstructions.objects

import core.transformation.{MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

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

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState,
                                          state: CompilationState): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "StoreAddress")
    InstructionSignature(Seq(stackTop), Seq())
  }

  override def getVariableUpdates(instruction: MetaObject, typeState: ProgramTypeState ): Map[Int, MetaObject] = {
    val variableLocation: Int = getInstructionArguments(instruction)(0)
    val _type = typeState.stackTypes.last
    Map(variableLocation -> _type)
  }

  object AddressStore

}
