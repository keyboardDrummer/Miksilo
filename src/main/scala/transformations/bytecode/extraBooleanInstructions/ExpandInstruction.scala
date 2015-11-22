package transformations.bytecode.extraBooleanInstructions

import core.particles.node.Node
import core.particles.{CompilationState, Contract, Particle}
import transformations.bytecode.coreInstructions.InstructionWithGrammar

trait ExpandInstruction extends Particle with InstructionWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpandVirtualInstructionsC)

  def expand(instruction: Node, state: CompilationState) : Seq[Node]

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ExpandVirtualInstructionsC.getState(state).expandInstruction.put(key, i => expand(i, state))
  }
}
