package transformations.bytecode.extraBooleanInstructions

import core.particles.node.Node
import core.particles.{Language, Contract, Delta}
import transformations.bytecode.coreInstructions.InstructionWithGrammar

trait ExpandInstruction extends Delta with InstructionWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpandVirtualInstructionsC)

  def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node]

  override def inject(state: Language): Unit = {
    super.inject(state)
    ExpandVirtualInstructionsC.getState(state).expandInstruction.put(key, this)
  }
}
