package deltas.bytecode.types

import core.bigrammar.grammars.Keyword
import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}

object CharTypeC extends TypeInstance
{
  object CharTypeKey extends NodeShape
  override val key = CharTypeKey
  val me = new Node(CharTypeKey)

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    "char" ~> value(me)
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("C",false) ~> value(me)
  }

  override def description: String = "Adds the char type."
}
