package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object SolidityTypeDelta extends DeltaWithGrammar {
  object Shape extends NodeShape
  object Name extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val grammar = identifier.as(Name) asLabelledNode Shape
  }

  override def description= "Introduce types to solidity"

  override def dependencies = Set.empty
}
