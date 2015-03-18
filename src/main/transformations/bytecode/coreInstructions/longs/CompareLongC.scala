package transformations.bytecode.coreInstructions.longs

import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.{IntTypeC, LongTypeC}

object CompareLongC extends InstructionC {

  val compareLong = new Node(CompareLongKey)

  object CompareLongKey

  override val key: AnyRef = CompareLongKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("94")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType, LongTypeC.longType), Seq(IntTypeC.intType))
}
