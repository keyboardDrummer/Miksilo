package transformations.bytecode.types


import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}

object ArrayTypeC extends TypeInstance with StackType {
  override val key = ArrayTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq(ObjectTypeDelta.rootObjectType)

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    "[" ~> typeGrammar.as(ArrayElementType) asNode ArrayTypeKey
  }

  def getArrayElementType(arrayType: Node): Node = arrayType(ArrayElementType).asInstanceOf[Node]

  override def getJavaGrammar(grammars: GrammarCatalogue): ArrayTypeC.NodeGrammar = {
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.as(ArrayElementType) ~< "[]" asNode ArrayTypeKey
  }

  def arrayType(elementType: Node): Node = {
    new Node(ArrayTypeKey, ArrayElementType -> elementType)
  }

  override def getStackSize: Int = 1

  object ArrayTypeKey extends NodeClass

  object ArrayElementType extends NodeField

  override def description: String = "Defines the array type."
}
