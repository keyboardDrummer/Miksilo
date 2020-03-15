package miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.{IntTypeDelta, LongTypeDelta}

object CompareLongDelta extends InstructionInstance {

  val compareLong = new Node(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(LongTypeDelta.longType, LongTypeDelta.longType), Seq(IntTypeDelta.intType))

  override def grammarName = "lcmp"
}
