package miksilo.modularLanguages.core.bigrammar

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.GrammarKey

class SelectGrammar(key: GrammarKey) extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    if (key != null)
      grammars.root.inner = grammars.find(key)
  }

  override def dependencies: Set[Contract] = Set.empty

  override def description: String = "Sets the program grammar to a specific grammar from the grammar catalogue."
}
