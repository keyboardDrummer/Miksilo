package deltas.bytecode.coreInstructions.integers.integerCompare

import core.language.node.{Key, Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeDelta

object IfIntegerCompareGreaterOrEqualDelta extends JumpInstruction {

  def ifIntegerCompareGreater(target: Int): Node = CodeAttributeDelta.instruction(shape, Seq(target))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("a2") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeDelta.intType, IntTypeDelta.intType), Seq())

  override def grammarName = "if_icmpge"
}
