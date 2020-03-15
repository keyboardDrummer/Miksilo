package miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object PushNullDelta extends InstructionInstance {

  val pushNull = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("01")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeDelta.intType))

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def grammarName = "aconst_null"
}
