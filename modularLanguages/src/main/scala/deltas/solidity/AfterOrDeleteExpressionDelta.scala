package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.expression.ExpressionDelta

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
