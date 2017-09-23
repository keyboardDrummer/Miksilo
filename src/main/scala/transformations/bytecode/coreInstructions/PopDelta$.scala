package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.simpleBytecode.ProgramTypeState

object PopDelta$ extends InstructionDelta {

  object PopKey extends Key
  override val key: Key = PopKey

  def pop = CodeAttribute.instruction(PopKey)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("57")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertSingleWord(state, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop instruction, which pops the top value from the stack."
}
