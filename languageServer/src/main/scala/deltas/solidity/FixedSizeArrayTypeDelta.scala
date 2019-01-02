package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object FixedSizeArrayTypeDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Size extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeDelta.Grammar)
    val grammar = typeGrammar.as(DynamicArrayTypeDelta.ElementType) ~<
      "[" ~ integer.as(Size) ~ "]" asNode Shape
    typeGrammar.addAlternative(grammar)
  }

  override def description = "Adds fixed size array types"

  override def dependencies = Set(TypeDelta)
}
