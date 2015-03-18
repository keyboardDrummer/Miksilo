package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node

object VoidTypeC extends TypeInstance {

  override val key: AnyRef = VoidTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getByteCodeString(_type: Node, state: CompilationState): String = "V"

  override def getStackSize: Int = 0

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "void" ~> produce(voidType)
  }

  def voidType = new Node(VoidTypeKey)

  object VoidTypeKey

  override def description: String = "Defines the void type."
}
