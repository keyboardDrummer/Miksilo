package deltas.smithy

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.expression.ExpressionDelta
import deltas.json.JsonObjectLiteralDelta
import deltas.{FileWithMembersDelta, HasNameDelta}

object TraitDelta extends DeltaWithGrammar {

  object TraitValueShape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val objectValue = find(JsonObjectLiteralDelta.Shape)
    val name = find(HasNameDelta.Name)
    val traitStatement = "trait" ~~ name ~~ objectValue
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(traitStatement)

    val shapeIdentifier = find(GenericSmithyDelta.ShapeIdentifierGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val text = find(GenericSmithyDelta.TextGrammar)
    val traitStructure = (text ~ ":" ~~ expression).manySeparatedVertical(",")
    val traitBody: BiGrammar = expression | traitStructure
    val traitValue = create(TraitValueShape, "@" ~ shapeIdentifier ~ traitBody.inParenthesis.option)
    create(Traits, traitValue.manyVertical.as(Traits))
  }

  object Traits extends NodeField

  override def description = "Adds trait statements and trait values"

  override def dependencies = Set(FileWithMembersDelta)
}
