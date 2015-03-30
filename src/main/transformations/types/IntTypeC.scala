package transformations.types

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.{Key, Node}

object IntTypeC extends TypeInstance with StackType {

  override val key = IntTypeKey

  override def getSuperTypes(_type: Node, state: CompilationState): Seq[Node] = Seq.empty //TODO extend. long ?


  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = "I" ~> produce(IntTypeC.intType)

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    "int" ~> produce(IntTypeC.intType)
  }

  def intType = new Node(IntTypeKey)

  override def getStackSize: Int = 1

  object IntTypeKey extends Key

  override def description: String = "Defines the integer type."
}
