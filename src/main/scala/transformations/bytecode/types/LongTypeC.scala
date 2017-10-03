package transformations.bytecode.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}

object LongTypeC extends TypeInstance with StackType {

  override val key = LongTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = new Keyword("J",false) ~> produce(longType)

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "long" ~> produce(longType)
  }

  val longType = new Node(LongTypeKey)

  object LongTypeKey extends NodeClass

  override def description: String = "Defines the long type."
}
