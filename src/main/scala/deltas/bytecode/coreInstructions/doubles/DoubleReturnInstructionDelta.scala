package deltas.bytecode.coreInstructions.doubles

import core.deltas.Contract
import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.DoubleTypeDelta

object DoubleReturnInstructionDelta extends InstructionInstance {

  def create: Node = CodeAttributeDelta.instruction(shape)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(DoubleTypeDelta.doubleType), Seq())

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("af")

  override def dependencies: Set[Contract] = super.dependencies ++ Set(DoubleTypeDelta)

  override def description: String = "Defines the double return instruction, which returns a double from the current method."

  override def grammarName = "dreturn"
}
