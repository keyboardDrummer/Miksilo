package transformations.bytecode.coreInstructions.floats

import core.particles.node.{Node, NodeClass}
import core.particles.{Compilation, Contract}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.FloatTypeC

object FloatReturnInstructionDelta extends InstructionDelta {

  override val key = FloatReturn

  def create: Node = CodeAttribute.instruction(FloatReturn)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(FloatTypeC.floatType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ae")

  object FloatReturn extends NodeClass

  override def dependencies: Set[Contract] = super.dependencies ++ Set(FloatTypeC)

  override def description: String = "Defines the float return instruction, which returns a float from the current method."

  override def grammarName = "freturn"
}
