package deltas.smithy

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.{FileWithMembersDelta, HasNameDelta}

object StructureStatementDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val name = find(HasNameDelta.Name)
    val shapeIdentifier = find(GenericSmithyDelta.ShapeIdentifierGrammar)
    val traits = find(TraitDelta.Traits)
    val structuredMember: BiGrammar = traits.option % name ~ ":" ~~ shapeIdentifier
    val trailingComma = ",".option
    val structureBody = name ~~ (structuredMember.manySeparatedVertical(",") ~ trailingComma).inBraces
    val grammar = "structure" ~~ structureBody
    val members = find(ShapeStatementDelta.ShapeBody)
    members.addAlternative(grammar)
  }

  override def description = "Adds structure statement delta"

  override def dependencies = Set(FileWithMembersDelta)
}
