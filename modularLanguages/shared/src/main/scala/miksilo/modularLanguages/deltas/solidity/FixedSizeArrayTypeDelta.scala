package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{PrimitiveType, Type, TypeApplication}
import miksilo.modularLanguages.deltas.bytecode.types.{TypeInstance, TypeSkeleton}
import miksilo.modularLanguages.deltas.solidity.DynamicArrayTypeDelta.ArrayType

object FixedSizeArrayTypeDelta extends DeltaWithGrammar with TypeInstance {

  object Shape extends NodeShape
  object Size extends NodeField

  override def description = "Adds fixed size array types"

  override def dependencies = Set(TypeSkeleton)

  implicit class FixedSizeType[T <: NodeLike](node: T) extends ArrayType[T](node) {
    def size: Int = node.getValue(Size).asInstanceOf[Int]
  }

  override def getJavaGrammar(grammars: LanguageGrammars): NodeGrammar = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    typeGrammar.as(DynamicArrayTypeDelta.ElementType) ~<
      "[" ~ integer.as(Size) ~ "]" asNode Shape
  }

  val typeConstructor = PrimitiveType("fixedSizeArray")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope) = {
    val arrayType: FixedSizeType[NodeLike] = path
    val elementType = TypeSkeleton.getType(compilation, builder, arrayType.elementType, parentScope)
    TypeApplication(typeConstructor, Seq[Type](elementType, PrimitiveType(arrayType.size)), path)
  }

  override def getSuperTypes(node: Node) = Seq.empty

  override def shape = Shape
}
