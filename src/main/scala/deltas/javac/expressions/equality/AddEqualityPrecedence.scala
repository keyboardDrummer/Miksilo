package deltas.javac.expressions.equality

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.GrammarKey
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton

object AddEqualityPrecedence extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val equalityGrammar = grammars.create(EqualityExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = equalityGrammar
  }

  object EqualityExpressionGrammar extends GrammarKey

  override def description: String = "Creates a named grammar with the correct precedence for equality-like operators."
}
