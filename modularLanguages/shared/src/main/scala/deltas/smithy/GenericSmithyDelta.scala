package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.GrammarKey
import deltas.expression.{ExpressionDelta, StringLiteralDelta}
import deltas.json.JsonObjectLiteralDelta.MemberKey
import deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta, SingleQuotedStringLiteralDelta}

object GenericSmithyDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    find(JsonObjectLiteralDelta.MemberKey).addAlternative(identifier.as(MemberKey))

    val plainString = identifier.as(JsonStringLiteralDelta.Value) asNode StringLiteralDelta.Shape
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(plainString)

    create(TextGrammar, plainString | find(StringLiteralDelta.Shape) | find(SingleQuotedStringLiteralDelta.Grammar))
  }

  object TextGrammar extends GrammarKey

  override def description = "A bag of Smithy related changes"

  override def dependencies = Set(SingleQuotedStringLiteralDelta, JsonStringLiteralDelta)
}


