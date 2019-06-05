package deltas.smithy

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.GrammarKey
import deltas.HasNameDelta
import deltas.expression.ExpressionDelta
import deltas.json.JsonObjectLiteralDelta.MemberKey
import deltas.json.{JsonObjectLiteralDelta, SingleQuotedStringLiteralDelta, StringLiteralDelta}

object GenericSmithyDelta extends DeltaWithGrammar {

  object ShapeIdentifierGrammar extends GrammarKey
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val relativeShapeId = identifier ~ ("$" ~ identifier).option
    val namespaceIdentifier = find(NamespaceDelta.Shape)
    val absoluteShapeId = namespaceIdentifier ~ "#" ~ relativeShapeId
    create(ShapeIdentifierGrammar, relativeShapeId | absoluteShapeId)

    find(JsonObjectLiteralDelta.MemberKey).addAlternative(identifier.as(MemberKey))

    val plainString = identifier.as(StringLiteralDelta.Value) asNode StringLiteralDelta.Shape
    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(plainString)

    create(TextGrammar, plainString | find(StringLiteralDelta.Shape) | find(SingleQuotedStringLiteralDelta.Grammar))
  }

  object TextGrammar extends GrammarKey

  override def description = ""

  override def dependencies = Set(NamespaceDelta, SingleQuotedStringLiteralDelta, StringLiteralDelta)
}


