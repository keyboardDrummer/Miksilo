package deltas.bytecode.coreInstructions.integers

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState

object IncrementIntegerDelta extends InstructionInstance {

  def integerIncrement(location: Int, amount: Int) = CodeAttributeDelta.instruction(shape, Seq(location, amount))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("84") ++
      byteToBytes(arguments(0)) ++
      byteToBytes(arguments(1))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq.empty, Seq.empty)

  override def getInstructionSize(compilation: Compilation): Int = 3

  override def description: String = "Defines the increment integer instruction, which increments an integer variable by a specific amount."

  override def grammarName = "iinc"
}
