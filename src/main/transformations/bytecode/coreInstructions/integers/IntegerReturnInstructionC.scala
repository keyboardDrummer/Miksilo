package transformations.bytecode.coreInstructions.integers

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.coreInstructions.InstructionC
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IntegerReturnInstructionC extends InstructionC {

  override val key: AnyRef = IntegerReturn

  def integerReturn: MetaObject = ByteCodeSkeleton.instruction(IntegerReturn)

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize(instruction: MetaObject): Int = 1

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState) = (Seq(IntTypeC.intType), Seq())

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("ac")

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = -1

  object IntegerReturn

}
