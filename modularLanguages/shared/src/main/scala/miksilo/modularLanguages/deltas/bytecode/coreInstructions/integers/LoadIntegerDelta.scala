package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.core.deltas.Contract
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object LoadIntegerDelta extends InstructionInstance {

  def load(location: Integer) = CodeAttributeDelta.instruction(shape, Seq(location))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    val location = arguments.head
    if (location > 3)
      hexToBytes("15") ++ byteToBytes(location)
    else
      byteToBytes(hexToInt("1a") + location)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(), Seq(IntTypeDelta.intType))

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeDelta)

  override def description: String = "Defines the load integer instruction, which loads an integer from a variable."

  override def grammarName = "iload" //TODO eigenlijk heb je ook nog iload_0 etc.. maar die zitten verbogen in deze Delta.
}
