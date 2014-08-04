package transformations.bytecode.instructions

import core.transformation._
import transformations.bytecode._
import transformations.javac.base.ConstantPool

trait InstructionC extends Injector {

  override def inject(state: TransformationState): Unit = {
    ByteCodeSkeleton.getInstructionSignatureRegistry(state).put(key, getInstructionInAndOutputs)
    ByteCodeSkeleton.getInstructionStackSizeModificationRegistry(state).put(key, getInstructionStackSizeModification)
    PrintByteCode.getBytesRegistry(state).put(key, getInstructionByteCode)
    ByteCodeSkeleton.getInstructionSizeRegistry(state).put(key, getInstructionSize)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  val key: Any

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject])

  def getInstructionSize: Int

  def getInstructionByteCode(instruction: MetaObject): Seq[Byte]

  def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int

  protected def binary(_type: MetaObject) = (Seq(_type, _type), Seq(_type))
}
