package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object DynamicArrayTypeDelta extends DeltaWithGrammar { // TODO merge with ArrayTypeDelta

  object Shape extends NodeShape
  object ElementType extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeDelta.Grammar)
    val grammar = typeGrammar.as(ElementType) ~< "[]" asNode Shape
    typeGrammar.addAlternative(grammar)
  }

  override def description = "Adds dynamic array types"

  override def dependencies = Set(TypeDelta)
}
