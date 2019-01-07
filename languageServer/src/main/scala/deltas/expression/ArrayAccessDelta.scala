package deltas.expression

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object ArrayAccessDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Target extends NodeField
  object Index extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = expression.as(Target) ~ "[" ~ expression.as(Index) ~ "]" asLabelledNode Shape
    expression.addAlternative(grammar)
  }

  override def description = "Enables accessing values from arrays"

  override def dependencies = Set(ExpressionDelta)
}
