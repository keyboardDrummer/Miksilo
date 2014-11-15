package transformations.bytecode.coreInstructions

import core.transformation.MetaObject
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton._
import PrintByteCode._

object InvokeSpecialC extends InvokeC {
  override val key: AnyRef = InvokeSpecialKey

  def invokeSpecial(location: Int): MetaObject = instruction(InvokeSpecialKey, Seq(location))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("b7") ++ shortToBytes(arguments(0))
  }

  object InvokeSpecialKey

}