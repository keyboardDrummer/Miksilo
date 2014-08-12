package transformations.bytecode.coreInstructions.integerCompare

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.classes.ConstantPool
import transformations.javac.types.IntTypeC

object IfZeroC extends JumpInstruction {
  override val key: AnyRef = IfZeroKey

  def ifZero(target: Int) = instruction(IfZeroKey, Seq(target))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("99") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(IntTypeC.intType), Seq())

  object IfZeroKey

}
