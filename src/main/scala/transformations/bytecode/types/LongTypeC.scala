package transformations.bytecode.types

import core.bigrammar.{Keyword, BiGrammar}
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}

object LongTypeC extends TypeInstance with StackType {

  override val key = LongTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = new Keyword("J",false) ~> produce(longType)

  override def getStackSize: Int = 2

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "long" ~> produce(longType)
  }

  val longType = new Node(LongTypeKey)

  object LongTypeKey extends Key

  override def description: String = "Defines the long type."
}
