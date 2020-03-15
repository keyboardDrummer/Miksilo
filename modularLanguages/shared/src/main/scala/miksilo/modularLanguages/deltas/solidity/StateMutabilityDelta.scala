package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.GrammarKey

object StateMutabilityDelta extends DeltaWithGrammar {
  object Grammar extends GrammarKey

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    create(Grammar, "pure" | "view" | "payable" | "constant")
  }

  override def description = "Defines the state mutability modifiers"

  override def dependencies = Set.empty
}
