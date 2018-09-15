package deltas.javac.expressions.relational

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.GrammarKey
import deltas.expressions.ExpressionDelta

object AddRelationalPrecedence extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    val relationalGrammar = grammars.create(RelationalExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = relationalGrammar
  }

  object RelationalExpressionGrammar extends GrammarKey

  override def description: String = "Creates a named grammar with the correct precedence for relational operators."
}
