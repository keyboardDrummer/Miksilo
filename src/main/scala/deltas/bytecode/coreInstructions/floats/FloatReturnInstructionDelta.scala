package deltas.bytecode.coreInstructions.floats

import core.deltas.node.{Node, NodeClass}
import core.deltas.{Contract, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.FloatTypeC

object FloatReturnInstructionDelta extends InstructionDelta {

  def create: Node = CodeAttributeDelta.instruction(key)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(FloatTypeC.floatType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ae")

  override def dependencies: Set[Contract] = super.dependencies ++ Set(FloatTypeC)

  override def description: String = "Defines the float return instruction, which returns a float from the current method."

  override def grammarName = "freturn"
}
