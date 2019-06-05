package deltas.smithy

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.HasNameDelta

object SimpleShapeDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Type extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val values: BiGrammar = "blob" | "boolean" | "string" | "byte" | "short" | "integer" | "long" | "float" |
      "double" | "bigInteger" | "bigDecimal" | "timestamp"

    val name = find(HasNameDelta.Name)
    val grammar = values.as(Type) ~~ name asLabelledNode Shape
    find(ShapeStatementDelta.ShapeBody).addAlternative(grammar)
 }

  override def description = "Adds simple shapes"

  override def dependencies = Set(ShapeStatementDelta)
}
