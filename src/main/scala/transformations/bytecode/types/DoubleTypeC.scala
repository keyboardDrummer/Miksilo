package transformations.bytecode.types

import core.bigrammar.{Keyword, BiGrammar}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}

object DoubleTypeC extends TypeInstance with StackType {

  override val key = DoubleTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = new Keyword("D",false) ~> produce(doubleType)

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "double" ~> produce(doubleType)
  }

  val doubleType = new Node(key)

  object DoubleTypeKey extends Key

  override def description: String = "Defines the double type."
}
