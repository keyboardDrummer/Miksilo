package transformations.bytecode.coreInstructions.integers

import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
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

  override def description: String = "Defines the add integers instruction, which adds the top two stack values together and places the result on the stack."
}
