package transformations.bytecode.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass}

object ByteTypeC extends TypeInstance
{
  object ByteTypeKey extends NodeClass
  override val key = ByteTypeKey
  val me = new Node(ByteTypeKey)

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = "byte" ~> value(me)

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = new Keyword("B",false) ~> value(me)

  override def description: String = "Adds the byte type."
}
