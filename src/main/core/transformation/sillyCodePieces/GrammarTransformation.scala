package core.transformation.sillyCodePieces

import core.grammar.GrammarWriter
import core.transformation.TransformationState
import core.transformation.grammars.GrammarCatalogue

trait GrammarTransformation extends Injector with GrammarWriter {
  def transformGrammars(grammars: GrammarCatalogue)

  override def enter(state: TransformationState): Unit = {
    super.enter(state)
    transformGrammars(state.grammarCatalogue)
  }
}
