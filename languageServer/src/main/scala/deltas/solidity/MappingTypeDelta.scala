package deltas.solidity

import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, TypeApplication}
import deltas.bytecode.types.{TypeInstance, TypeSkeleton}

object MappingTypeDelta extends TypeInstance {
  object Shape extends NodeShape
  object Key extends NodeField
  object Value extends NodeField

  override def description = "Adds the mapping type"

  implicit class MappingType[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def key = node(Key).asInstanceOf[T]
    def value = node(Value).asInstanceOf[T]
  }

  override def dependencies = Set(ElementaryTypeDelta, TypeSkeleton)

  override def getSuperTypes(_type: Node) = Seq.empty

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    val elementaryType = find(ElementaryTypeDelta.Shape)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    "mapping" ~~ (elementaryType.as(Key) ~~ "=>" ~~ typeGrammar.as(Value)).inParenthesis asNode Shape
  }

  val mappingTypeFunctor = PrimitiveType("mapping")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope) = {
    val mappingType: MappingType[NodeLike] = path
    TypeApplication(mappingTypeFunctor, Seq(
      TypeSkeleton.getType(compilation, builder, mappingType.key, parentScope),
      TypeSkeleton.getType(compilation, builder, mappingType.value, parentScope)), path)
  }

  override def shape = Shape
}
