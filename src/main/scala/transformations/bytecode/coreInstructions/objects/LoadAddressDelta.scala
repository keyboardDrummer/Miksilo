package transformations.bytecode.coreInstructions.objects

import core.particles.Compilation
import core.particles.node.{Node, NodeClass}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState

object LoadAddressDelta extends InstructionDelta {

  override val key = AddressLoad

  def addressLoad(location: Int): Node = CodeAttribute.instruction(AddressLoad, Seq(location))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      PrintByteCode.hexToBytes("19") ++ PrintByteCode.byteToBytes(location)
    else
      PrintByteCode.byteToBytes(PrintByteCode.hexToInt("2a") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    val location = arguments(0)

    InstructionSignature(Seq(), Seq(typeState.variableTypes(location)))
  }

  object AddressLoad extends NodeClass

  override def grammarName = "aload" //TODO aload_0 etc..
}
