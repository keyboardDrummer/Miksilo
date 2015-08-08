package transformations.bytecode.types

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}

object ShortTypeC extends TypeInstance with StackType {

  override val key = ShortTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = Seq.empty //TODO extend. long ?


  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = "S" ~> produce(shortType)

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "short" ~> produce(shortType)
  }

  def shortType = new Node(ShortTypeKey)

  override def getStackSize: Int = 1

  object ShortTypeKey extends Key

  override def description: String = "Defines the short type."
}
