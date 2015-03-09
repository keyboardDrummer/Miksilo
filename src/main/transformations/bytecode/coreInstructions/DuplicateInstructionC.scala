package transformations.bytecode.coreInstructions

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.Instruction
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool



object DuplicateInstructionC extends InstructionC with Instruction {

  object DuplicateKey
  def duplicate = instruction(DuplicateKey, Seq.empty)

  override val key: AnyRef = DuplicateKey

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("59")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val input: MetaObject = typeState.stackTypes.last
    assertSingleWord(state, input)
    new InstructionSignature(Seq(input),Seq(input, input))
  }

  override def description: String = "Defines the duplicate instruction, which duplicates the top stack value."
}
