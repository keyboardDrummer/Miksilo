package transformations.bytecode.types

import core.bigrammar.{Keyword, BiGrammar}
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}

object VoidTypeC extends TypeInstance with StackType {

  override val key = VoidTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = ???

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = new Keyword("V",false) ~> produce(voidType)

  override def getStackSize: Int = 0

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "void" ~> produce(voidType)
  }

  def voidType = new Node(VoidTypeKey)

  object VoidTypeKey extends Key

  override def description: String = "Defines the void type."
}
