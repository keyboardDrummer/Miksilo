package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.simpleBytecode.ProgramTypeState

object VoidReturnInstructionDelta$ extends InstructionDelta {

  override val key: Key = VoidReturn

  def voidReturn: Node = CodeAttribute.instruction(VoidReturn)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("b1")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(), Seq())

  override def getInstructionSize: Int = 1

  object VoidReturn extends Key

  override def description: String = "Defines the void return instruction, which returns from the current method."
}
