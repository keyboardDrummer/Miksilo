package deltas.bytecode.coreInstructions.integers.integerCompare

import core.deltas.{Compilation, Language}
import core.deltas.node.{Key, Node, NodeClass}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object IfIntegerCompareGreaterOrEqualDelta extends JumpInstruction {

  def ifIntegerCompareGreater(target: Int): Node = CodeAttributeDelta.instruction(key, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("a2") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  override def grammarName = "if_icmpge"
}
