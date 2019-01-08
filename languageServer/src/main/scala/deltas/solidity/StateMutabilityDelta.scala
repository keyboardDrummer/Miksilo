package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.GrammarKey

object StateMutabilityDelta extends DeltaWithGrammar {
  object Grammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    grammars.create(Grammar, "pure" | "view" | "payable" | "constant")
  }

  override def description = "Defines the state mutability modifiers"

  override def dependencies = Set.empty
}
