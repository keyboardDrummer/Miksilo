package miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InstructionSignature
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta

object IfIntegerCompareLessOrEqualDelta extends JumpInstruction { //TODO superclasse maken om wat van deze jump instructies onder te schuiven

  def create(target: Int): Node = CodeAttributeDelta.instruction(shape, Seq(target))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    hexToBytes("a4") ++ shortToBytes(arguments.head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(IntTypeDelta.intType, IntTypeDelta.intType), Seq())

  override def grammarName = "if_icmple"
}
