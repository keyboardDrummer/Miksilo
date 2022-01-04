package miksilo.modularLanguages.deltas.statement

import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}

object RemoveGotoAndLabelStatementDelta extends DeltaWithGrammar {

  override def description: String = "Removes the go to and label statements"

  override def dependencies: Set[Contract] = Set(GotoStatementDelta, LabelStatementDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    grammars.findPath(StatementDelta.Grammar, GotoStatementDelta.Shape).removeMeFromOption()
    grammars.findPath(StatementDelta.Grammar, LabelStatementDelta.JavaLabelGrammar).removeMeFromOption()
  }
}
