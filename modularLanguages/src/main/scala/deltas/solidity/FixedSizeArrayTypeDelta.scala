package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type, TypeApplication}
import deltas.bytecode.types.{TypeInstance, TypeSkeleton}
import deltas.solidity.DynamicArrayTypeDelta.ArrayType

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
