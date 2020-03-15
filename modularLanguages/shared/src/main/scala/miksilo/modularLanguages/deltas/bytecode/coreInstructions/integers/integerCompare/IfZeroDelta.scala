package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.core.deltas.Contract
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InstructionSignature
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object IfZeroDelta extends JumpInstruction {

  def ifZero(target: Int) = CodeAttributeDelta.instruction(shape, Seq(target))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("99") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeDelta.intType), Seq())

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeDelta)

  override def grammarName = "ifeq"
}
