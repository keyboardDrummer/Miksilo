package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

object IntegerReturnC extends InstructionC {

  override val key: Any = IntegerReturn

  def integerReturn: MetaObject = ByteCodeSkeleton.instruction(IntegerReturn)

  override def getInstructionSize: Int = 1

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(JavaTypes.intType), Seq())

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("ac")

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = -1

  object IntegerReturn

}
