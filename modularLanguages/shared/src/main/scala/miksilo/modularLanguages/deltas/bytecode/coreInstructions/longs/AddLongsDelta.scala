package miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.LongTypeDelta

object AddLongsDelta extends InstructionInstance {

  def addLongs() = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = hexToBytes("61")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = binary(LongTypeDelta.longType)

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeDelta)

  override def description: String = "Defines the add longs instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "ladd"
}
