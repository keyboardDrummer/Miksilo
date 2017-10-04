package transformations.bytecode.coreInstructions.integers

import core.particles.node.{Node, NodeClass}
import core.particles.{Compilation, Contract}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object IntegerReturnInstructionDelta extends InstructionDelta {

  override val key = IntegerReturn

  def integerReturn: Node = CodeAttribute.instruction(IntegerReturn)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ac")

  object IntegerReturn extends NodeClass

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the integer return instruction, which returns an integer from the current method."

  override def grammarName = "ireturn"
}
