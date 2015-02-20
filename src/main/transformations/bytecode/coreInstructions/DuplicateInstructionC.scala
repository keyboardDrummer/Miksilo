package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.Instruction
import transformations.javac.classes.ConstantPool

object DuplicateInstructionC extends InstructionC with Instruction {

  object DuplicateKey
  def duplicate = instruction(DuplicateKey, Seq.empty)

  override val key: AnyRef = DuplicateKey

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("59")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, stackTypes: Seq[MetaObject],
                                          state: TransformationState): InstructionSignature = {
    val input: MetaObject = stackTypes.last
    assertSingleWord(state, input)
    new InstructionSignature(Seq(input),Seq(input, input))
  }
}
