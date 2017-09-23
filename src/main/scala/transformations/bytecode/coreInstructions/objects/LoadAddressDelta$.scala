package transformations.bytecode.coreInstructions.objects

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState

object LoadAddressDelta$ extends InstructionDelta {

  override val key: Key = AddressLoad

  def addressLoad(location: Int): Node = CodeAttribute.instruction(AddressLoad, Seq(location))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      PrintByteCode.hexToBytes("19") ++ PrintByteCode.byteToBytes(location)
    else
      PrintByteCode.byteToBytes(PrintByteCode.hexToInt("2a") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)

    InstructionSignature(Seq(), Seq(typeState.variableTypes(location)))
  }

  object AddressLoad extends Key

}
