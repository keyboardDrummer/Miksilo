package transformations.bytecode.coreInstructions.longs

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.LongTypeC

object LongReturnInstructionDelta extends InstructionDelta {

  override val key: Key = LongReturn

  def longReturn: Node = CodeAttribute.instruction(LongReturn)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ad")

  object LongReturn extends Key

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeC)

  override def description: String = "Defines the long return instruction, which returns a long from the current method."

  override def grammarName = "lreturn"
}
