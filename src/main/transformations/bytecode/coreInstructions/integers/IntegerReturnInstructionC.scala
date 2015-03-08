package transformations.bytecode.coreInstructions.integers

import core.particles.{Contract, MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool
import transformations.types.IntTypeC

object IntegerReturnInstructionC extends InstructionC {

  override val key: AnyRef = IntegerReturn

  def integerReturn: MetaObject = ByteCodeSkeleton.instruction(IntegerReturn)

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize(instruction: MetaObject): Int = 1

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("ac")

  object IntegerReturn

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the integer return instruction, which returns an integer from the current method."
}
