package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object AddIntegersDelta extends InstructionInstance {

  def addIntegers() = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = hexToBytes("60")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = binary(IntTypeDelta.intType)

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeDelta)

  override def description: String = "Defines the add integers instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "iadd"
}
