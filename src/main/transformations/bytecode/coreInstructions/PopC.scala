package transformations.bytecode.coreInstructions

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.classes.ConstantPool

object PopC extends InstructionC {

  object PopKey
  override val key: AnyRef = PopKey

  def pop = ByteCodeSkeleton.instruction(PopKey)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("57")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val input: MetaObject = typeState.stackTypes.last
    assertSingleWord(state, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop instruction, which pops the top value from the stack."
}
