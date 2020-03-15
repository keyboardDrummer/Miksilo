package miksilo.modularLanguages.deltas.javac.expressions.equality

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.GrammarKey
import miksilo.modularLanguages.deltas.expression.ExpressionDelta

object AddEqualityPrecedence extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    val equalityGrammar = grammars.create(EqualityExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = equalityGrammar
  }

  object EqualityExpressionGrammar extends GrammarKey

  override def description: String = "Creates a named grammar with the correct precedence for equality-like operators."
}
