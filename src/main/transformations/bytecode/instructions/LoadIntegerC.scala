package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

object LoadIntegerC extends InstructionC {

  override val key: Any = IntegerLoad

  def integerLoad(location: Integer) = ByteCodeSkeleton.instruction(IntegerLoad, Seq(location))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    val location = arguments(0)
    if (location > 3)
      hexToBytes("15") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("1a") + location)
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(), Seq(JavaTypes.intType))

  override def getInstructionSize: Int = 1

  object IntegerLoad

}
