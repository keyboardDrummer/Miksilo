package transformations.bytecode.coreInstructions

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.attributes.CodeAttribute.JumpBehavior
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool

object GotoC extends InstructionC {

  override val key: AnyRef = GoToKey

  def goTo(target: Int): MetaObject = CodeAttribute.instruction(GoToKey, Seq(target))

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("a7") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, true)

  override def getSignature(instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(), Seq())

  override def getInstructionSize: Int = 3

  object GoToKey

  override def description: String = "Defines the goto instruction, which jumps to a target instruction."
}
