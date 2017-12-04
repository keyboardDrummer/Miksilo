package deltas.bytecode.coreInstructions.longs

import core.deltas.Language
import core.deltas.node.Node
import deltas.bytecode.PrintByteCode
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.{IntTypeC, LongTypeC}

object CompareLongDelta extends InstructionDelta {

  val compareLong = new Node(key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType, LongTypeC.longType), Seq(IntTypeC.intType))

  override def grammarName = "lcmp"
}
