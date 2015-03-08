package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, CompilationState}
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object Pop2C extends InstructionC {

  object Pop2Key
  override val key: AnyRef = Pop2Key

  def pop2 = ByteCodeSkeleton.instruction(Pop2Key)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("58")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val input: MetaObject = typeState.stackTypes.last
    assertDoubleWord(state, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop2 instruction, which pops the top two values from the stack."
}
