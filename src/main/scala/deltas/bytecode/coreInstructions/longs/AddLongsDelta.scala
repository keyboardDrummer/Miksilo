package deltas.bytecode.coreInstructions.longs

import core.deltas.node.{Key, Node, NodeShape}
import core.deltas.{Compilation, Contract}
import core.language.Language
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeC

object AddLongsDelta extends InstructionDelta {

  def addLongs() = CodeAttributeDelta.instruction(key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = hexToBytes("61")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = binary(LongTypeC.longType)

  override def getInstructionSize: Int = 1

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeC)

  override def description: String = "Defines the add longs instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "ladd"
}
