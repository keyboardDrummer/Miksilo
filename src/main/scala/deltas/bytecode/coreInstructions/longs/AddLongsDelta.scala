package deltas.bytecode.coreInstructions.longs

import core.deltas.Contract
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeDelta

object AddLongsDelta extends InstructionInstance {

  def addLongs() = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = hexToBytes("61")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = binary(LongTypeDelta.longType)

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeDelta)

  override def description: String = "Defines the add longs instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "ladd"
}
