package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.PrintByteCode
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object PopC extends InstructionC {

  object PopKey
  override val key: AnyRef = PopKey

  def pop = new MetaObject(PopKey)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    PrintByteCode.hexToBytes("57")
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState)
    = (Seq(IntTypeC.intType),Seq())
}
