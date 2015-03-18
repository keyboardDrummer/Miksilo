package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool



object DuplicateInstructionC extends InstructionC {

  object DuplicateKey
  def duplicate = CodeAttribute.instruction(DuplicateKey, Seq.empty)

  override val key: AnyRef = DuplicateKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("59")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertSingleWord(state, input)
    new InstructionSignature(Seq(input),Seq(input, input))
  }

  override def description: String = "Defines the duplicate instruction, which duplicates the top stack value."
}
