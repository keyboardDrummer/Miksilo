package transformations.bytecode

import core.transformation.MetaObject
import transformations.bytecode.LabelledTargets.LabelKey
import transformations.javac.base.ConstantPool

object Instructions {

  def getInstructionInputTypes(constantPool: ConstantPool, instruction: MetaObject): Seq[MetaObject] =
    getInstructionInAndOutputs(constantPool, instruction)._1

  def getInstructionOutputTypes(constantPool: ConstantPool, instruction: MetaObject): Seq[MetaObject] =
    getInstructionInAndOutputs(constantPool, instruction)._2

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject]) =
    instruction.clazz match {
      case LabelKey => (Seq(), Seq())
    }
}
