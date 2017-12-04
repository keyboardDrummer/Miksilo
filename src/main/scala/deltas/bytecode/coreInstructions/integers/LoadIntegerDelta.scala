package deltas.bytecode.coreInstructions.integers

import core.deltas.node.Node
import core.deltas.{Contract, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object LoadIntegerDelta extends InstructionDelta {

  def load(location: Integer) = CodeAttributeDelta.instruction(key, Seq(location))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments.head
    if (location > 3)
      hexToBytes("15") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("1a") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the load integer instruction, which loads an integer from a variable."

  override def grammarName = "iload" //TODO eigenlijk heb je ook nog iload_0 etc.. maar die zitten verbogen in deze Delta.
}
