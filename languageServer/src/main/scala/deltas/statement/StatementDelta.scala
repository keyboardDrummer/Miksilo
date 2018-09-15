package deltas.statement

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.GrammarKey

object StatementDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    grammars.create(Grammar)
  }

  object Grammar extends GrammarKey

  override def description: String = "Defines the concept of a statement."
}
