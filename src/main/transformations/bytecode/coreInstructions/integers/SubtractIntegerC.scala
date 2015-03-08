package transformations.bytecode.coreInstructions.integers

import core.particles.{Contract, MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object SubtractIntegerC extends InstructionC {
  override val key: AnyRef = SubtractIntegerKey

  def subtractInteger = instruction(SubtractIntegerKey)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = hexToBytes("64")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = binary(IntTypeC.intType)

  override def getInstructionSize(instruction: MetaObject): Int = 1

  object SubtractIntegerKey
  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the subtract integer instruction, which subtracts the top two integer on the stack and places the result on the stack."
}
