package deltas.bytecode.coreInstructions.longs

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeDelta

object LoadLongDelta extends InstructionInstance {

  def load(location: Integer) = CodeAttributeDelta.instruction(shape, Seq(location))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("16") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("1e") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(), Seq(LongTypeDelta.longType))

  override def grammarName = "lload" //TODO lload_0 etc..
}

