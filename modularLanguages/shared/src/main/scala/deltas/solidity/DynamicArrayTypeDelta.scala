package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import deltas.bytecode.types.TypeSkeleton

object DynamicArrayTypeDelta extends DeltaWithGrammar { // TODO merge with ArrayTypeDelta

  object Shape extends NodeShape
  object ElementType extends NodeField

  implicit class ArrayType[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def elementType: T = node(ElementType).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val grammar = typeGrammar.as(ElementType) ~< "[]" asNode Shape
    typeGrammar.addAlternative(grammar)
  }

  override def description = "Adds dynamic array types"

  override def dependencies = Set(TypeSkeleton)
}
