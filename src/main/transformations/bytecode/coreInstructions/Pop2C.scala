package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.MetaObject
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object Pop2C extends InstructionC {

  object Pop2Key
  override val key: AnyRef = Pop2Key

  def pop2 = CodeAttribute.instruction(Pop2Key)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("58")
  }

  override def getSignature(instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val input: MetaObject = typeState.stackTypes.last
    assertDoubleWord(state, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop2 instruction, which pops the top two values from the stack."
}
