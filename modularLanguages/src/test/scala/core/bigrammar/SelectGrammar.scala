package core.bigrammar

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.GrammarKey

class SelectGrammar(key: GrammarKey) extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    if (key != null)
      grammars.root.inner = grammars.find(key)
  }

  override def dependencies: Set[Contract] = Set.empty

  override def description: String = "Sets the program grammar to a specific grammar from the grammar catalogue."
}
