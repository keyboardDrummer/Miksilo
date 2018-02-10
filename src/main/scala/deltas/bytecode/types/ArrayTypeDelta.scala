package deltas.bytecode.types


import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField, NodeLike, NodeShape}
import core.deltas.{Compilation, NodeGrammar}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type, TypeApplication}

object ArrayTypeDelta extends ByteCodeTypeInstance with StackType {
  override val shape = ArrayTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq(QualifiedObjectTypeDelta.rootObjectType)

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    "[" ~> typeGrammar.as(ArrayElementType) asNode ArrayTypeKey
  }

  def getElementType[T <: NodeLike](arrayType: T): T = arrayType(ArrayElementType).asInstanceOf[T]

  override def getJavaGrammar(grammars: LanguageGrammars): NodeGrammar = {
    import grammars._
    val parseType = find(TypeSkeleton.JavaTypeGrammar)
    parseType.as(ArrayElementType) ~< "[]" asNode ArrayTypeKey
  }

  def arrayType(elementType: Node): Node = {
    new Node(ArrayTypeKey, ArrayElementType -> elementType)
  }

  override def getStackSize: Int = 1

  object ArrayTypeKey extends NodeShape

  object ArrayElementType extends NodeField

  override def description: String = "Defines the array type."

  val arrayType = PrimitiveType("Array")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val elementType = TypeSkeleton.getType(compilation, builder, _type(ArrayElementType).asInstanceOf[NodeLike], parentScope)
    TypeApplication(arrayType, Seq(elementType), _type)
  }
}
