package transformations.bytecode.coreInstructions.integers

import core.transformation.{MetaObject, CompilationState}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton._
import PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IntegerConstantC extends InstructionC {

  override val key: AnyRef = IntegerConstantKey

  def integerConstant(value: Int) = {
    require (value <= 5)
    require (value >= -1)
    instruction(IntegerConstantKey, Seq(value))
  }

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    byteToBytes(3 + ByteCodeSkeleton.getInstructionArguments(instruction)(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize(instruction: MetaObject): Int = 1

  private object IntegerConstantKey

  override def description: String = "Defines the integer constant instruction, which places an integer between -1 and 5 on the stack."
}
