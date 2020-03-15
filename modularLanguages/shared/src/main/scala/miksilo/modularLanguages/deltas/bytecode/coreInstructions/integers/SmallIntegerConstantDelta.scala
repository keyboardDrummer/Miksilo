package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object SmallIntegerConstantDelta extends InstructionInstance {

  def integerConstant(value: Int) = {
    require (value <= 5)
    require (value >= -1)
    CodeAttributeDelta.instruction(shape, Seq(value))
  }

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    byteToBytes(3 + CodeAttributeDelta.getInstructionArguments(instruction).head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(), Seq(IntTypeDelta.intType))

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def description: String = "Defines the integer constant instruction, which places an integer between -1 and 5 on the stack."

  override def grammarName = "iconst" //TODO eigenlijk heb je ook nog iconst_0 etc.. maar die zitten verbogen in deze Delta.
}
