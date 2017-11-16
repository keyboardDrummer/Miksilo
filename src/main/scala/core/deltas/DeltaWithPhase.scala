package core.deltas

import core.deltas.node.Node

trait DeltaWithPhase extends Delta {
  def transform(program: Node, state: Compilation): Unit

  override def inject(state: Language): Unit = {
    super.inject(state)
    state.compilerPhases ::= new Phase(this.name, description, compilation => transform(compilation.program, compilation))
  }
}
