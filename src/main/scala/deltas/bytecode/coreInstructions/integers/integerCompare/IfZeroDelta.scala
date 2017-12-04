package deltas.bytecode.coreInstructions.integers.integerCompare

import core.deltas.node.Node
import core.deltas.{Contract, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object IfZeroDelta extends JumpInstruction {

  def ifZero(target: Int) = CodeAttributeDelta.instruction(key, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("99") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def grammarName = "ifeq"
}
