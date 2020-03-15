package miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.core.deltas.{Contract, Delta, HasShape}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InstructionWithGrammar

trait ExpandInstruction extends Delta with InstructionWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpandVirtualInstructionsDelta)

  def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node]

  override def inject(language: Language): Unit = {
    super.inject(language)
    ExpandVirtualInstructionsDelta.expandInstruction.add(language, this)
  }
}
