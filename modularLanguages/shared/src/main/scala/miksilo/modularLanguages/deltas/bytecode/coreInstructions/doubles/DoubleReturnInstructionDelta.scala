package miksilo.modularLanguages.deltas.bytecode.coreInstructions.doubles

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.DoubleTypeDelta

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
