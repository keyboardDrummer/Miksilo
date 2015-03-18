package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node

object BooleanTypeC extends TypeInstance {
  override val key: AnyRef = BooleanTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = Seq.empty

  override def getByteCodeString(_type: Node, state: CompilationState): String = "Z"


  override def getStackType(_type: Node, state: CompilationState): Node = IntTypeC.intType

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "boolean" ~> produce(booleanType)
  }

  def booleanType = new Node(BooleanTypeKey)

  override def getStackSize: Int = 1

  object BooleanTypeKey

  override def description: String = "Defines the boolean type."
}
