package core.deltas

import core.deltas.path.PathRoot
import core.language.node.Node
import core.language.{Compilation, Language, Phase}

trait DeltaWithPhase extends Delta {
  def transformProgram(program: Node, compilation: Compilation): Unit

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.compilerPhases ::= Phase(this, this.description,
      compilation => {
        val rootNode = if (compilation.program != null)
            compilation.program.asInstanceOf[PathRoot].current
          else
            null
        transformProgram(rootNode, compilation)
      })
  }
}
