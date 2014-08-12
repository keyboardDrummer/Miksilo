package transformations.bytecode.coreInstructions.integerCompare

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IfIntegerCompareLessC extends JumpInstruction {

  override val key: AnyRef = IfIntegerCompareLessKey

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction(IfIntegerCompareLessKey, Seq(target))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("a1") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject]) =
    (Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object IfIntegerCompareLessKey

}
