package miksilo.modularLanguages.deltas.expression.multiplicative

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.GrammarKey
import miksilo.modularLanguages.deltas.expression.ExpressionDelta

object MultiplicativePrecedenceDelta extends DeltaWithGrammar {

  override def description: String = "Creates a named grammar with the correct precedence for multiplication-like operators."

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = grammars.create(Grammar, expressionGrammar.inner)
    expressionGrammar.inner = grammar
  }

  object Grammar extends GrammarKey
}
