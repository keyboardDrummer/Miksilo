package deltas.bytecode.extraBooleanInstructions

import core.language.node.Node
import core.deltas.{Contract, Delta, HasShape}
import core.language.Language
import deltas.bytecode.coreInstructions.InstructionWithGrammar

trait ExpandInstruction extends Delta with InstructionWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpandVirtualInstructionsDelta)

  def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node]

  override def inject(language: Language): Unit = {
    super.inject(language)
    ExpandVirtualInstructionsDelta.expandInstruction.add(language, this)
  }
}
