package core.deltas

import core.deltas.node.Node
import core.language.{Language, Phase}

trait DeltaWithPhase extends Delta {
  def transformProgram(program: Node, compilation: Compilation): Unit

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.compilerPhases ::= Phase(this, compilation => transformProgram(compilation.program, compilation))
  }
}
