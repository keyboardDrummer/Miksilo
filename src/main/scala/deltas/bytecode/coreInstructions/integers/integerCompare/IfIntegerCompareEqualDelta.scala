package deltas.bytecode.coreInstructions.integers.integerCompare

import core.deltas.Language
import core.deltas.node.{Node, NodeShape}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object IfIntegerCompareEqualDelta extends JumpInstruction {

  def ifIntegerCompareGreater(target: Int): Node = CodeAttributeDelta.instruction(key, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("9f") ++ shortToBytes(arguments(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  override def description: String = "Defines the if-integer-compare-equal instruction, which will to a target instruction if the two top stack integers are equal."

  override def grammarName = "if_icmpeq"
}
