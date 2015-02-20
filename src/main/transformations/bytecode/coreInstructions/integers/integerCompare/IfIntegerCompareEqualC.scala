package transformations.bytecode.coreInstructions.integers.integerCompare

import core.transformation.{TransformationState, MetaObject}
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton._
import PrintByteCode._
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IfIntegerCompareEqualC extends JumpInstruction {

  override val key: AnyRef = IfIntegerCompareEqualKey

  def ifIntegerCompareGreater(target: Int): MetaObject = instruction(IfIntegerCompareEqualKey, Seq(target))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("9f") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, stackTypes: Seq[MetaObject],
                                          state: TransformationState): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object IfIntegerCompareEqualKey

}
