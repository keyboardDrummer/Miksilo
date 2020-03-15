package miksilo.modularLanguages.core.deltas

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language

trait DeltaWithGrammar extends Delta {

  def transformGrammars(grammars: LanguageGrammars, language: Language): Unit

  override def inject(language: Language): Unit = {
    super.inject(language)
    transformGrammars(LanguageGrammars.grammars.get(language), language)
  }
}
