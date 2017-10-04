package transformations.bytecode.coreInstructions.integers.integerCompare

import core.particles.Compilation
import core.particles.node.{Node, NodeClass}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object IfIntegerCompareLessDelta extends JumpInstruction {

  override val key = IfIntegerCompareLessKey

  def ifIntegerCompareGreater(target: Int): Node = CodeAttribute.instruction(IfIntegerCompareLessKey, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("a1") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object IfIntegerCompareLessKey extends NodeClass

  override def grammarName = "if_icmplt"
}
