package deltas.bytecode.coreInstructions.longs

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta}

object CompareLongDelta extends InstructionInstance {

  val compareLong = new Node(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(LongTypeDelta.longType, LongTypeDelta.longType), Seq(IntTypeDelta.intType))

  override def grammarName = "lcmp"
}
