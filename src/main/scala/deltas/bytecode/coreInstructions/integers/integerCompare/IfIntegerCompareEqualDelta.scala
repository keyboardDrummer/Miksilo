package deltas.bytecode.coreInstructions.integers.integerCompare

import core.language.node.{Node, NodeShape}
import core.language.Language
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeDelta

object IfIntegerCompareEqualDelta extends JumpInstruction {

  def ifIntegerCompareGreater(target: Int): Node = CodeAttributeDelta.instruction(key, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("9f") ++ shortToBytes(arguments(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeDelta.intType, IntTypeDelta.intType), Seq())

  override def description: String = "Defines the if-integer-compare-equal instruction, which will to a target instruction if the two top stack integers are equal."

  override def grammarName = "if_icmpeq"
}
