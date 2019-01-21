package deltas.expression.additive

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.GrammarKey
import deltas.expression.ExpressionDelta

object AdditivePrecedenceDelta extends DeltaWithGrammar {

  override def description: String = "Creates a named grammar with the correct precedence for addition-like operators."

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    val additiveGrammar = grammars.create(Grammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
  }

  object Grammar extends GrammarKey
}
