package deltas.bytecode.types

import core.bigrammar.grammars.Keyword
import core.bigrammar.BiGrammar
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}

object ByteTypeC extends TypeInstance
{
  object ByteTypeKey extends NodeClass
  override val key = ByteTypeKey
  val me = new Node(ByteTypeKey)

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = ???

  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    "byte" ~> value(me)
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("B",false) ~> value(me)
  }

  override def description: String = "Adds the byte type."
}
