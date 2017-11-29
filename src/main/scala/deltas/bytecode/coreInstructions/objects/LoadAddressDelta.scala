package deltas.bytecode.coreInstructions.objects

import core.deltas.{Compilation, Language}
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState

object LoadAddressDelta extends InstructionDelta {

  override val key = AddressLoad

  def addressLoad(location: Int): Node = CodeAttributeDelta.instruction(AddressLoad, Seq(location))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      PrintByteCode.hexToBytes("19") ++ PrintByteCode.byteToBytes(location)
    else
      PrintByteCode.byteToBytes(PrintByteCode.hexToInt("2a") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)

    InstructionSignature(Seq(), Seq(typeState.variableTypes(location)))
  }

  object AddressLoad extends NodeClass

  override def grammarName = "aload" //TODO aload_0 etc..
}
