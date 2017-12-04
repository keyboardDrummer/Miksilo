package deltas.bytecode.coreInstructions.integers.integerCompare

import core.deltas.Language
import core.deltas.node.Node
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object IfNotZero extends JumpInstruction {

  def ifZero(target: Int) = CodeAttributeDelta.instruction(key, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("9a") ++ shortToBytes(arguments(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def grammarName = "ifne"
}
