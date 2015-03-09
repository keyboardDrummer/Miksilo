package transformations.bytecode.coreInstructions.integers

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}
import transformations.javac.classes.ConstantPool

object IncrementIntegerC extends InstructionC {

  override val key: AnyRef = IntegerIncrementKey

  def integerIncrement(location: Int, amount: Int) = ByteCodeSkeleton.instruction(IntegerIncrementKey, Seq(location, amount))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("84") ++
      byteToBytes(arguments(0)) ++
      byteToBytes(arguments(1))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq.empty, Seq.empty)

  override def getInstructionSize(): Int = 3

  object IntegerIncrementKey

  override def description: String = "Defines the increment integer instruction, which increments an integer variable by a specific amount."
}
