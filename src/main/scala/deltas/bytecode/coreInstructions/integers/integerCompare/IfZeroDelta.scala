package deltas.bytecode.coreInstructions.integers.integerCompare

import core.deltas.node.{Key, Node, NodeClass}
import core.deltas.{Compilation, Contract, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.coreInstructions.InstructionSignature
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object IfZeroDelta extends JumpInstruction {
  override val key = Clazz

  def ifZero(target: Int) = CodeAttribute.instruction(Clazz, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("99") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeC.intType), Seq())

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  object Clazz extends NodeClass

  override def grammarName = "ifeq"
}
