package transformations.bytecode.coreInstructions.integers.integerCompare

import core.particles.{Compilation, Language}
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.InstructionSignature
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object IfIntegerCompareGreaterDelta$ extends JumpInstruction {

  override val key: Key = IfIntegerCompareGreaterKey

  def ifIntegerCompareGreater(target: Int): Node = CodeAttribute.instruction(IfIntegerCompareGreaterKey, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("a3") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  object IfIntegerCompareGreaterKey extends Key

  override def grammarName = "if_icmpgt"
}
