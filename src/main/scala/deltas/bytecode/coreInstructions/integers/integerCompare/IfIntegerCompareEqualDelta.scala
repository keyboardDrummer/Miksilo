package deltas.bytecode.coreInstructions.integers.integerCompare

import core.deltas.Compilation
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

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
