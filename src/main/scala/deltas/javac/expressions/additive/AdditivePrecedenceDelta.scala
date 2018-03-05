package deltas.javac.expressions.additive

import core.deltas.grammars.LanguageGrammars
import core.language.node.GrammarKey
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton

object AdditivePrecedenceDelta extends DeltaWithGrammar {

  override def description: String = "Creates a named grammar with the correct precedence for addition-like operators."

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val additiveGrammar = grammars.create(Grammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
  }

  object Grammar extends GrammarKey
}
