package deltas.bytecode.coreInstructions.longs

import core.deltas.node.Node
import core.language.Language
import deltas.bytecode.PrintByteCode
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta}

object CompareLongDelta extends InstructionDelta {

  val compareLong = new Node(key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(LongTypeDelta.longType, LongTypeDelta.longType), Seq(IntTypeDelta.intType))

  override def grammarName = "lcmp"
}
