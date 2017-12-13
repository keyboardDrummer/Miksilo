package deltas.bytecode.types


import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape, NodeField}
import core.deltas.{Language, NodeGrammar}

object ArrayTypeC extends TypeInstance with StackType {
  override val key = ArrayTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq(ObjectTypeDelta.rootObjectType)

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    "[" ~> typeGrammar.as(ArrayElementType) asNode ArrayTypeKey
  }

  def getArrayElementType(arrayType: Node): Node = arrayType(ArrayElementType).asInstanceOf[Node]

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
}
