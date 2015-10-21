package transformations.bytecode.extraBooleanInstructions

import core.particles.node.Node
import core.particles.{CompilationState, Contract, Particle}

trait ExpandInstruction extends Particle {
  def key: Any

  override def dependencies: Set[Contract] = Set(ExpandVirtualInstructionsC)

  def expand(instruction: Node, state: CompilationState) : Seq[Node]

  override def inject(state: CompilationState): Unit = {
    ExpandVirtualInstructionsC.getState(state).expandInstruction.put(key, i => expand(i, state))
  }
}
