package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.GrammarKey

object TypeDelta extends DeltaWithGrammar { // TOOD merge with TypeSkeleton

  object Grammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    grammars.create(Grammar)
  }

  override def description= "Introduce types to solidity"

  override def dependencies = Set.empty
}

