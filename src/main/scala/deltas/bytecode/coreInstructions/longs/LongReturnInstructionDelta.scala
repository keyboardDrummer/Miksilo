package deltas.bytecode.coreInstructions.longs

import core.language.node.Node
import core.deltas.Contract
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeDelta

object LongReturnInstructionDelta extends InstructionInstance {

  def longReturn: Node = CodeAttributeDelta.instruction(shape)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(LongTypeDelta.longType), Seq())

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ad")

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeDelta)

  override def description: String = "Defines the long return instruction, which returns a long from the current method."

  override def grammarName = "lreturn"
}
