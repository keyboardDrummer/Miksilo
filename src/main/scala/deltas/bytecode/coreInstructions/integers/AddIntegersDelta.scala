package deltas.bytecode.coreInstructions.integers

import core.deltas.node.{Key, Node, NodeClass}
import core.deltas.{Compilation, Contract, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object AddIntegersDelta extends InstructionDelta {

  def addIntegers() = CodeAttributeDelta.instruction(key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = hexToBytes("60")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = binary(IntTypeC.intType)

  override def getInstructionSize: Int = 1

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the add integers instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "iadd"
}
