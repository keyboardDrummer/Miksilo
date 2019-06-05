package deltas.smithy

import core.bigrammar.grammars.Keyword
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.HasNameDelta

object SmithyListDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val name = find(HasNameDelta.Name)
    val traits = find(TraitDelta.Traits)
    val shapeIdentifier = find(GenericSmithyDelta.ShapeIdentifierGrammar)
    val listBody = name ~ (traits ~ "member" ~ ":" ~~ shapeIdentifier).inBraces
    val grammar = Keyword("list", reserved = false) ~~ listBody
    find(ShapeStatementDelta.ShapeBody).addAlternative(grammar)
  }

  override def description = "Adds the list statement"

  override def dependencies = Set.empty
}
