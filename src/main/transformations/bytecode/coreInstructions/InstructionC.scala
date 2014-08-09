package transformations.bytecode.coreInstructions

import core.transformation._
import core.transformation.sillyCodePieces.Injector
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode._
import transformations.javac.base.ConstantPool

trait InstructionC extends Injector {

  override def inject(state: TransformationState): Unit = {
    ByteCodeSkeleton.getInstructionSignatureRegistry(state).put(key, getInstructionInAndOutputs)
    ByteCodeSkeleton.getInstructionStackSizeModificationRegistry(state).put(key, (c, i) => getInstructionStackSizeModification(c, i, state))
    PrintByteCode.getBytesRegistry(state).put(key, getInstructionByteCode)
    ByteCodeSkeleton.getInstructionSizeRegistry(state).put(key, getInstructionSize)
    ByteCodeSkeleton.getState(state).jumpBehaviorRegistry.put(key, getJumpBehavior)
    ByteCodeSkeleton.getState(state).localUpdates.put(key, getVariableUpdates)
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  val key: AnyRef

  def getVariableUpdates(instruction: MetaObject): Map[Int, MetaObject] = Map.empty

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject])

  def getInstructionSize(instruction: MetaObject): Int = getInstructionByteCode(instruction).size

  def getJumpBehavior: JumpBehavior = new JumpBehavior(true, false)

  def getInstructionByteCode(instruction: MetaObject): Seq[Byte]

  def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = {
    val inAndOutputs = getInstructionInAndOutputs(constantPool, instruction)
    inAndOutputs._2.size - inAndOutputs._1.size
  }


  protected def binary(_type: MetaObject) = (Seq(_type, _type), Seq(_type))
}
