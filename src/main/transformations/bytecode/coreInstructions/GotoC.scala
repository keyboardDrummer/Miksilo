package transformations.bytecode.coreInstructions

import core.particles.{MetaObject, CompilationState}
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.javac.classes.ConstantPool

object GotoC extends InstructionC {

  override val key: AnyRef = GoToKey

  def goTo(target: Int): MetaObject = ByteCodeSkeleton.instruction(GoToKey, Seq(target))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("a7") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def getJumpBehavior: JumpBehavior = new JumpBehavior(false, true)

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(), Seq())

  override def getInstructionSize(instruction: MetaObject): Int = 3

  object GoToKey

  override def description: String = "Defines the goto instruction, which jumps to a target instruction."
}
