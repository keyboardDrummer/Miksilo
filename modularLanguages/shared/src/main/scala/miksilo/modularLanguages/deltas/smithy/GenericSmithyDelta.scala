package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.node.GrammarKey
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, StringLiteralDelta}
import miksilo.modularLanguages.deltas.json.JsonObjectLiteralDelta.MemberKey
import miksilo.modularLanguages.deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta, SingleQuotedStringLiteralDelta}

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


