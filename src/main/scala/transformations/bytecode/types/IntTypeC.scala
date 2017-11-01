package transformations.bytecode.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}

object IntTypeC extends TypeInstance with StackType {

  override val key = IntTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq.empty //TODO extend. long ?

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    import grammars._
    new Keyword("I", false) ~> value(intType)
  }

  override def getJavaGrammar(grammars: GrammarCatalogue) = {
    import grammars._
    "int" ~> value(intType)
  }

  val intType = new Node(IntTypeKey)

  override def getStackSize: Int = 1

  object IntTypeKey extends NodeClass

  override def description: String = "Defines the integer type."
}
