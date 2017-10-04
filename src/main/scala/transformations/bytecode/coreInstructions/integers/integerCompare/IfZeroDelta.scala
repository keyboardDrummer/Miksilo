package transformations.bytecode.coreInstructions.integers.integerCompare

import core.particles.node.{Key, Node, NodeClass}
import core.particles.{Compilation, Contract}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object IfZeroDelta extends JumpInstruction {
  override val key = Clazz

  def ifZero(target: Int) = CodeAttribute.instruction(Clazz, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("99") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  object Clazz extends NodeClass

  override def grammarName = "ifeq"
}
