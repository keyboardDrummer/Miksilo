package deltas.bytecode.coreInstructions.longs

import core.particles.Compilation
import core.particles.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.{IntTypeC, LongTypeC}

object CompareLongDelta extends InstructionDelta {

  val compareLong = new Node(CompareLongKey)

  object CompareLongKey extends NodeClass

  override val key = CompareLongKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType, LongTypeC.longType), Seq(IntTypeC.intType))

  override def grammarName = "lcmp"
}
