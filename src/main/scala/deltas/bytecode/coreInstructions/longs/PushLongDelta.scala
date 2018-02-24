package deltas.bytecode.coreInstructions.longs

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeDelta

object PushLongDelta extends InstructionInstance {

  def constant(value: Int) = {
    require (0 <= value && value <= 1)
    CodeAttributeDelta.instruction(shape, Seq(value))
  }

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    byteToBytes(9 + CodeAttributeDelta.getInstructionArguments(instruction).head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(), Seq(LongTypeDelta.longType))

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def grammarName = "lconst" //TODO lconst_0 & lconst_1
}

