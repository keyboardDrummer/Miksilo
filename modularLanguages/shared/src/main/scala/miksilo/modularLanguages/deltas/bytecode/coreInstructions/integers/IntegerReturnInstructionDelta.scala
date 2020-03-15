package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object IntegerReturnInstructionDelta extends InstructionInstance {

  def integerReturn: Node = CodeAttributeDelta.instruction(shape)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize(compilation: Compilation): Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(IntTypeDelta.intType), Seq())

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ac")

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeDelta)

  override def description: String = "Defines the integer return instruction, which returns an integer from the current method."

  override def grammarName = "ireturn"
}
