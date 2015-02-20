package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.PrintByteCode
import transformations.javac.classes.ConstantPool

object Pop2C extends InstructionC {

  object Pop2Key
  override val key: AnyRef = Pop2Key

  def pop2 = new MetaObject(Pop2Key)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("58")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, stackTypes: Seq[MetaObject],
                                          state: TransformationState): InstructionSignature = {
    val input: MetaObject = stackTypes.last
    assertDoubleWord(state, input)
    InstructionSignature(Seq(input),Seq())
  }
}
