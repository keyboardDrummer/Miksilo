package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

object IfZeroC extends InstructionC {
  override val key: Any = IfZeroKey

  def ifZero(target: Int) = instruction(IfZeroKey, Seq(target))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = -1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("99") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject]) = (Seq(JavaTypes.intType), Seq())

  override def getInstructionSize: Int = 3

  object IfZeroKey

}
