package transformations.bytecode.coreInstructions

import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.Instruction
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object DuplicateInstructionC extends InstructionC with Instruction {

  object DuplicateKey
  def duplicate = instruction(DuplicateKey, Seq.empty)

  override val key: AnyRef = DuplicateKey

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("59")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState):
  (Seq[MetaObject], Seq[MetaObject]) = {
    (Seq(IntTypeC.intType),Seq(IntTypeC.intType, IntTypeC.intType))
  }
}
