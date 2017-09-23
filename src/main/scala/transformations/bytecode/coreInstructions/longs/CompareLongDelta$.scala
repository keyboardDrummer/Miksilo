package transformations.bytecode.coreInstructions.longs

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.{IntTypeC, LongTypeC}

object CompareLongDelta$ extends InstructionDelta {

  val compareLong = new Node(CompareLongKey)

  object CompareLongKey extends Key

  override val key: Key = CompareLongKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType, LongTypeC.longType), Seq(IntTypeC.intType))
}
