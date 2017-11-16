package deltas.bytecode.extraBooleanInstructions

import core.deltas.node.Node
import core.deltas.{Language, Contract, Delta}
import deltas.bytecode.coreInstructions.InstructionWithGrammar

trait ExpandInstruction extends Delta with InstructionWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpandVirtualInstructionsC)

  def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node]

  override def inject(state: Language): Unit = {
    super.inject(state)
    ExpandVirtualInstructionsC.getRegistry(state).expandInstruction.put(key, this)
  }
}
