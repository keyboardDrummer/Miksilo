package transformations.types


import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node

object ArrayTypeC extends TypeInstance {
  override val key: AnyRef = ArrayTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = Seq.empty

  override def getByteCodeString(_type: Node, state: CompilationState): String =
    s"[${TypeSkeleton.getByteCodeString(state)(getArrayElementType(_type))}"

  def getArrayElementType(arrayType: Node) = arrayType(ArrayElementType).asInstanceOf[Node]

  override def getJavaGrammar(grammars: GrammarCatalogue)= {
    val parseType = grammars.find(TypeSkeleton.TypeGrammar)
     parseType <~ "[]" ^^ parseMap(ArrayTypeKey, ArrayElementType)
  }

  def arrayType(elementType: Node) = {
    new Node(ArrayTypeKey, ArrayElementType -> elementType)
  }

  override def getStackSize: Int = 1


  object ArrayTypeKey

  object ArrayElementType

  override def description: String = "Defines the array type."
}
