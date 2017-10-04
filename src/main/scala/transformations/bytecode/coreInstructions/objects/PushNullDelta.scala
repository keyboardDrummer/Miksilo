package transformations.bytecode.coreInstructions.objects

import core.particles.Compilation
import core.particles.node.{Node, NodeClass}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object PushNullDelta extends InstructionDelta {

  override val key = PushNullKey
  val pushNull = CodeAttribute.instruction(key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("01")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize: Int = 1

  object PushNullKey extends NodeClass

  override def grammarName = "aconst_null"
}
