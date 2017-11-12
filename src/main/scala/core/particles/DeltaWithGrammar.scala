package core.particles

import core.particles.grammars.LanguageGrammars

trait DeltaWithGrammar extends Delta with NodeGrammarWriter {

  def transformGrammars(grammars: LanguageGrammars, state: Language): Unit

  override def inject(state: Language): Unit = {
    super.inject(state)
    transformGrammars(state.grammars, state)
  }
}
