package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node

object LongTypeC extends TypeInstance {

  override val key: AnyRef = LongTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getByteCodeString(_type: Node, state: CompilationState): String = "J"

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "long" ~> produce(longType)
  }

  def longType = new Node(LongTypeKey)

  object LongTypeKey

  override def description: String = "Defines the long type."
}
