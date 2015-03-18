package transformations.types

import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node

object DoubleTypeC extends TypeInstance {

  override val key: AnyRef = DoubleTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getByteCodeString(_type: Node, state: CompilationState): String = "D"

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "double" ~> produce(doubleType)
  }

  val doubleType = new Node(key)

  object DoubleTypeKey

  override def description: String = "Defines the double type."
}
