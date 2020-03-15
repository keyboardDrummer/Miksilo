package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.expression.ExpressionDelta

object AfterOrDeleteExpressionDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = ("after" | "delete") ~~ expression
    expression.addAlternative(grammar)
  }

  override def description = "Add after and delete expression"

  override def dependencies = Set(ExpressionDelta)
}
