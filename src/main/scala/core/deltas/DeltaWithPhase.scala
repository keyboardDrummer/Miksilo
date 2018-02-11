package core.deltas

import core.language.node.Node
import core.language.{Compilation, Language, Phase}

trait DeltaWithPhase extends Delta {
  def transformProgram(program: Node, compilation: Compilation): Unit

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.compilerPhases ::= Phase(this, compilation => transformProgram(compilation.program, compilation))
  }
}
