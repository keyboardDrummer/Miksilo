package transformations.bytecode.extraBooleanInstructions

import core.particles.node.Node
import core.particles.{CompilationState, Contract, Delta}
import transformations.bytecode.coreInstructions.InstructionWithGrammar

trait ExpandInstruction extends Delta with InstructionWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpandVirtualInstructionsC)

  def expand(instruction: Node, state: CompilationState) : Seq[Node]

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ExpandVirtualInstructionsC.getState(state).expandInstruction.put(key, i => expand(i, state))
  }
}
