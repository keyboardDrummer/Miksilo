package transformations.bytecode.coreInstructions

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object Duplicate2InstructionC extends InstructionC {

  object Duplicate2Key
  def duplicate = CodeAttribute.instruction(Duplicate2Key, Seq.empty)

  override val key: AnyRef = Duplicate2Key

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("5c")
  }

  override def getSignature(instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val input: MetaObject = typeState.stackTypes.last
    assertDoubleWord(state, input)
    new InstructionSignature(Seq(input),Seq(input, input))
  }

  override def description: String = "Defines the duplicate2 instruction, which duplicates the top two stack values."
}
