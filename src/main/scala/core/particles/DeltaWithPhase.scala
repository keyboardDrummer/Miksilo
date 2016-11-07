package core.particles

import core.particles.node.Node

trait DeltaWithPhase extends Delta {
  def transform(program: Node, state: CompilationState)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    state.compilerPhases ::= new Phase(this.name, description, () => transform(state.program, state))
  }
}
