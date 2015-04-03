package transformations.types

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, Key}

object ByteTypeC extends TypeInstance
{
  object ByteTypeKey extends Key
  override val key: Key = ByteTypeKey
  val me = new Node(ByteTypeKey)

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = "byte" ~> produce(me)

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = "B" ~> produce(me)

  override def description: String = "Adds the byte type."
}
