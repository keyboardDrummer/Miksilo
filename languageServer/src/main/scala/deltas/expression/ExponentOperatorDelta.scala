package deltas.expression

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language

object ExponentOperatorDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = expression ~~ "**" ~~ expression
    expression.addAlternative(grammar)
  }

  override def description = "Adds the exponent operator **"

  override def dependencies = Set(ExpressionDelta)
}
