package deltas.bytecode.types


import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type, TypeApplication}

object ArrayTypeDelta extends ByteCodeTypeInstance with HasStackTypeDelta {
  override val shape = Shape

  override def getSuperTypes(_type: Node): Seq[Node] = Seq(QualifiedObjectTypeDelta.rootObjectType)

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    "[" ~> typeGrammar.as(ArrayElementType) asNode Shape
  }

  def getElementType[T <: NodeLike](arrayType: T): T = arrayType(ArrayElementType).asInstanceOf[T]

  override def getJavaGrammar(grammars: LanguageGrammars): NodeGrammar = {
    import grammars._
    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    parseType.as(ArrayElementType) ~< "[]" asNode Shape
  }

  def arrayType(elementType: Node): Node = {
    new Node(Shape, ArrayElementType -> elementType)
  }

  override def getStackSize: Int = 1

  object Shape extends NodeShape

  object ArrayElementType extends NodeField

  override def description: String = "Defines the array type."

  val arrayType = PrimitiveType("Array")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val elementType = TypeSkeleton.getType(compilation, builder, _type(ArrayElementType).asInstanceOf[NodeLike], parentScope)
    TypeApplication(arrayType, Seq(elementType), _type)
  }
}
