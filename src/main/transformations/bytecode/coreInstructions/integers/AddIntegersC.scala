package transformations.bytecode.coreInstructions.integers

import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode
import PrintByteCode._
import transformations.bytecode.PrintByteCode
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object AddIntegersC extends InstructionC {
  override val key: AnyRef = AddIntegersKey

  def addInteger = instruction(AddIntegersKey)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = hexToBytes("60")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: TransformationState): InstructionSignature = binary(IntTypeC.intType)

  override def getInstructionSize(instruction: MetaObject): Int = 1

  object AddIntegersKey

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)
}
