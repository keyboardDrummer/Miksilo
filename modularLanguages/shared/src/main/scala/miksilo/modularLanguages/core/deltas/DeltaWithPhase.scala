package miksilo.modularLanguages.core.deltas

import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language, Phase}

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
