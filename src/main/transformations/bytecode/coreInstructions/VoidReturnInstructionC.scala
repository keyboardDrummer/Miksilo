package transformations.bytecode.coreInstructions

import core.particles.{MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool

object VoidReturnInstructionC extends InstructionC {

  override val key: AnyRef = VoidReturn

  def voidReturn: MetaObject = ByteCodeSkeleton.instruction(VoidReturn)

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = PrintByteCode.hexToBytes("b1")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(), Seq())

  override def getInstructionSize(instruction: MetaObject): Int = 1

  object VoidReturn

  override def description: String = "Defines the void return instruction, which returns from the current method."
}
