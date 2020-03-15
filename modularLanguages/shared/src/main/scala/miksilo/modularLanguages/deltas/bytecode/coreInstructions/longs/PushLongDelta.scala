package miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.LongTypeDelta

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

