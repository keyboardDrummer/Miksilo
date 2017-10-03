package transformations.bytecode.types

import core.bigrammar.{Keyword, BiGrammar}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, Key}

object ByteTypeC extends TypeInstance
{
  object ByteTypeKey extends Key
  override val key: Key = ByteTypeKey
  val me = new Node(ByteTypeKey)

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = "byte" ~> produce(me)

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = new Keyword("B",false) ~> produce(me)

  override def description: String = "Adds the byte type."
}
