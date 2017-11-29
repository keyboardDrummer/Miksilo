package deltas.bytecode.coreInstructions.objects

import core.deltas.{Compilation, Language}
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState

object StoreAddressDelta extends InstructionDelta {
  override val key = AddressStore

  def addressStore(location: Int): Node = CodeAttributeDelta.instruction(AddressStore, Seq(location))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("3a") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("4b") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val stackTop = typeState.stackTypes.last
    assertObjectTypeStackTop(stackTop, "StoreAddress")
    InstructionSignature(Seq(stackTop), Seq())
  }

  override def getVariableUpdates(instruction: Node, typeState: ProgramTypeState ): Map[Int, Node] = {
    val variableLocation: Int = CodeAttributeDelta.getInstructionArguments(instruction)(0)
    val _type = typeState.stackTypes.last
    Map(variableLocation -> _type)
  }

  object AddressStore extends NodeClass

  override def grammarName = "astore" //TODO astore_0 etc..
}
