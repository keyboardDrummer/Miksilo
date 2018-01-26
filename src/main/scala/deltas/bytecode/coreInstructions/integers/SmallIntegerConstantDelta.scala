package deltas.bytecode.coreInstructions.integers

import core.deltas.node.Node
import core.language.Language
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object SmallIntegerConstantDelta extends InstructionDelta {

  def integerConstant(value: Int) = {
    require (value <= 5)
    require (value >= -1)
    CodeAttributeDelta.instruction(key, Seq(value))
  }

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    byteToBytes(3 + CodeAttributeDelta.getInstructionArguments(instruction).head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize: Int = 1

  override def description: String = "Defines the integer constant instruction, which places an integer between -1 and 5 on the stack."

  override def grammarName = "iconst" //TODO eigenlijk heb je ook nog iconst_0 etc.. maar die zitten verbogen in deze Delta.
}
