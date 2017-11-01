package transformations.bytecode.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}

object CharTypeC extends TypeInstance
{
  object CharTypeKey extends NodeClass
  override val key = CharTypeKey
  val me = new Node(CharTypeKey)

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar = {
    import grammars._
    "char" ~> value(me)
  }

  override def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar = {
    import grammars._
    new Keyword("C",false) ~> value(me)
  }

  override def description: String = "Adds the char type."
}
