package core.deltas

import core.deltas.node.Node

trait DeltaWithPhase extends Delta {
  def transformProgram(program: Node, state: Compilation): Unit

  override def inject(state: Language): Unit = {
    super.inject(state)
    state.compilerPhases ::= new Phase(this.name, description, compilation => transformProgram(compilation.program, compilation))
  }
}
