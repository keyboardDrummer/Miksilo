package core.deltas

import core.deltas.grammars.LanguageGrammars

trait DeltaWithGrammar extends Delta with NodeGrammarWriter {

  def transformGrammars(grammars: LanguageGrammars, language: Language): Unit

  override def inject(language: Language): Unit = {
    super.inject(language)
    transformGrammars(language.grammars, language)
  }
}
