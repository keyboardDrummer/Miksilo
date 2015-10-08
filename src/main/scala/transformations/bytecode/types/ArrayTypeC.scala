package transformations.bytecode.types


import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}

object ArrayTypeC extends TypeInstance with StackType {
  override val key = ArrayTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    val typeGrammar = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    val grammar: BiGrammar = "[" ~> typeGrammar ^^ parseMap(ArrayTypeKey, ArrayElementType)
    grammar
  }

  def getArrayElementType(arrayType: Node) = arrayType(ArrayElementType).asInstanceOf[Node]

  override def getJavaGrammar(grammars: GrammarCatalogue)= {
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
     parseType <~ "[]" ^^ parseMap(ArrayTypeKey, ArrayElementType)
  }

  def arrayType(elementType: Node) = {
    new Node(ArrayTypeKey, ArrayElementType -> elementType)
  }

  override def getStackSize: Int = 1

  object ArrayTypeKey extends Key

  object ArrayElementType

  override def description: String = "Defines the array type."
}
