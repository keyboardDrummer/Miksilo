package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object ObjectTypeDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Parts extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val grammar = identifier.someSeparated(".").as(Parts) asLabelledNode Shape
  }

  override def description = "Adds qualified object types"

  override def dependencies = Set(SolidityTypeDelta)
}
