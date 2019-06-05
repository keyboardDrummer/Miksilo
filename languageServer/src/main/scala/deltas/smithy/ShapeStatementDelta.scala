package deltas.smithy

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.FileWithMembersDelta
import deltas.smithy.TraitDelta.TraitValueShape

// TODO change this so that traits inject themselves
object ShapeStatementDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object ShapeBody extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val traits = find(TraitDelta.Traits)
    val shapeBody = create(ShapeBody).as(ShapeBody)
    val grammar = traits % shapeBody asLabelledNode Shape
    val members = find(FileWithMembersDelta.Members)
    members.addAlternative(grammar)
  }

  override def description = "Adds the shape statement concept"

  override def dependencies = Set(FileWithMembersDelta, TraitDelta)
}
