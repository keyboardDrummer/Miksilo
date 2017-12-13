package deltas.bytecode.coreInstructions.objects

import core.deltas.{Compilation, Language}
import core.deltas.node.{Node, NodeShape}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object PushNullDelta extends InstructionDelta {

  val pushNull = CodeAttributeDelta.instruction(key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("01")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize: Int = 1

  override def grammarName = "aconst_null"
}
