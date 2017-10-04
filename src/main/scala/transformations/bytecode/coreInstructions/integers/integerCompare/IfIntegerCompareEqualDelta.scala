package transformations.bytecode.coreInstructions.integers.integerCompare

import core.particles.Compilation
import core.particles.node.{Node, NodeClass}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object IfIntegerCompareEqualDelta extends JumpInstruction {

  override val key = Clazz

  def ifIntegerCompareGreater(target: Int): Node = CodeAttribute.instruction(Clazz, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("9f") ++ shortToBytes(arguments(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object Clazz extends NodeClass

  override def description: String = "Defines the if-integer-compare-equal instruction, which will to a target instruction if the two top stack integers are equal."

  override def grammarName = "if_icmpeq"
}
