package core.deltas

import core.bigrammar.textMate.BiGrammarToTextMate.jsonLanguage
import core.deltas.grammars.LanguageGrammars
import core.language.Language

trait DeltaWithGrammar extends Delta {

  def transformGrammars(grammars: LanguageGrammars, language: Language): Unit

  override def inject(language: Language): Unit = {
    super.inject(language)
    transformGrammars(LanguageGrammars.grammars.get(language), language)
  }
}
