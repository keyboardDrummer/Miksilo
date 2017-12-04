package deltas.bytecode.coreInstructions.integers.integerCompare

import core.deltas.Language
import core.deltas.node.Node
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object IfIntegerCompareLessOrEqualDelta extends JumpInstruction { //TODO superclasse maken om wat van deze jump instructies onder te schuiven

  def create(target: Int): Node = CodeAttributeDelta.instruction(key, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("a4") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType, IntTypeC.intType), Seq())

  override def grammarName = "if_icmple"
}
